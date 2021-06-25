#include "eager_search.h"

#include "../evaluation_context.h"
#include "../evaluator.h"
#include "../open_list_factory.h"
#include "../option_parser.h"
#include "../pruning_method.h"

#include "../algorithms/ordered_set.h"
#include "../task_utils/successor_generator.h"

#include "../utils/logging.h"

#include "../unsolvability/unsolvabilitymanager.h"

#include <cassert>
#include <cstdlib>
#include <memory>
#include <optional.hh>
#include <set>

using namespace std;

namespace eager_search {
EagerSearch::EagerSearch(const Options &opts)
    : SearchEngine(opts),
      reopen_closed_nodes(opts.get<bool>("reopen_closed")),
      unsolv_type(opts.get<UnsolvabilityVerificationType>("unsolv_verification")),
      open_list(opts.get<shared_ptr<OpenListFactory>>("open")->
                create_state_open_list()),
      f_evaluator(opts.get<shared_ptr<Evaluator>>("f_eval", nullptr)),
      preferred_operator_evaluators(opts.get_list<shared_ptr<Evaluator>>("preferred")),
      lazy_evaluator(opts.get<shared_ptr<Evaluator>>("lazy_evaluator", nullptr)),
      pruning_method(opts.get<shared_ptr<PruningMethod>>("pruning")),
      unsolvability_directory(opts.get<std::string>("unsolv_directory")) {
    if (lazy_evaluator && !lazy_evaluator->does_cache_estimates()) {
        cerr << "lazy_evaluator must cache its estimates" << endl;
        utils::exit_with(utils::ExitCode::SEARCH_INPUT_ERROR);
    }

    if(unsolv_type != UnsolvabilityVerificationType::NONE) {
        if(unsolvability_directory.compare(".") == 0) {
            unsolvability_directory = "";
        }
        // expand environment variables
        size_t found = unsolvability_directory.find('$');
        while(found != std::string::npos) {
            size_t end = unsolvability_directory.find('/');
            std::string envvar;
            if(end == std::string::npos) {
                envvar = unsolvability_directory.substr(found+1);
            } else {
                envvar = unsolvability_directory.substr(found+1,end-found-1);
            }
            // to upper case
            for(size_t i = 0; i < envvar.size(); i++) {
                envvar.at(i) = toupper(envvar.at(i));
            }
            std::string expanded = std::getenv(envvar.c_str());
            unsolvability_directory.replace(found,envvar.length()+1,expanded);
            found = unsolvability_directory.find('$');
        }
        if(!unsolvability_directory.empty() && !(unsolvability_directory.back() == '/')) {
            unsolvability_directory += "/";
        }
        std::cout << "Generating unsolvability verification in "
                  << unsolvability_directory << std::endl;
        if (unsolv_type == UnsolvabilityVerificationType::PROOF_DISCARD) {
            CuddManager::set_compact_proof(false);
        } else if (unsolv_type == UnsolvabilityVerificationType::PROOF) {
            CuddManager::set_compact_proof(true);
        }
    }
}

void EagerSearch::initialize() {
    utils::g_log << "Conducting best first search"
                 << (reopen_closed_nodes ? " with" : " without")
                 << " reopening closed nodes, (real) bound = " << bound
                 << endl;
    assert(open_list);

    set<Evaluator *> evals;
    open_list->get_path_dependent_evaluators(evals);

    /*
      Collect path-dependent evaluators that are used for preferred operators
      (in case they are not also used in the open list).
    */
    for (const shared_ptr<Evaluator> &evaluator : preferred_operator_evaluators) {
        evaluator->get_path_dependent_evaluators(evals);
    }

    /*
      Collect path-dependent evaluators that are used in the f_evaluator.
      They are usually also used in the open list and will hence already be
      included, but we want to be sure.
    */
    if (f_evaluator) {
        f_evaluator->get_path_dependent_evaluators(evals);
    }

    /*
      Collect path-dependent evaluators that are used in the lazy_evaluator
      (in case they are not already included).
    */
    if (lazy_evaluator) {
        lazy_evaluator->get_path_dependent_evaluators(evals);
    }

    path_dependent_evaluators.assign(evals.begin(), evals.end());

    State initial_state = state_registry.get_initial_state();
    for (Evaluator *evaluator : path_dependent_evaluators) {
        evaluator->notify_initial_state(initial_state);
    }

    /*
      Note: we consider the initial state as reached by a preferred
      operator.
    */
    EvaluationContext eval_context(initial_state, 0, true, &statistics);

    statistics.inc_evaluated_states();

    if (open_list->is_dead_end(eval_context)) {
        if (unsolv_type == UnsolvabilityVerificationType::PROOF ||
                   unsolv_type == UnsolvabilityVerificationType::PROOF_DISCARD) {
            open_list->store_deadend_info(eval_context);
        }
        utils::g_log << "Initial state is a dead end." << endl;
    } else {
        if (search_progress.check_progress(eval_context))
            statistics.print_checkpoint_line(0);
        start_f_value_statistics(eval_context);
        SearchNode node = search_space.get_node(initial_state);
        node.open_initial();

        open_list->insert(eval_context, initial_state.get_id());
    }

    print_initial_evaluator_values(eval_context);

    pruning_method->initialize(task);
}

void EagerSearch::print_statistics() const {
    statistics.print_detailed_statistics();
    search_space.print_statistics();
    pruning_method->print_statistics();
}

SearchStatus EagerSearch::step() {
    tl::optional<SearchNode> node;
    while (true) {
        if (open_list->empty()) {
            if(unsolv_type == UnsolvabilityVerificationType::PROOF ||
                    unsolv_type == UnsolvabilityVerificationType::PROOF_DISCARD) {
                write_unsolvability_proof();
            }
            utils::g_log << "Completely explored state space -- no solution!" << endl;
            return FAILED;
        }
        StateID id = open_list->remove_min();
        State s = state_registry.lookup_state(id);
        node.emplace(search_space.get_node(s));

        if (node->is_closed())
            continue;

        /*
          We can pass calculate_preferred=false here since preferred
          operators are computed when the state is expanded.
        */
        EvaluationContext eval_context(s, node->get_g(), false, &statistics);

        if (lazy_evaluator) {
            /*
              With lazy evaluators (and only with these) we can have dead nodes
              in the open list.

              For example, consider a state s that is reached twice before it is expanded.
              The first time we insert it into the open list, we compute a finite
              heuristic value. The second time we insert it, the cached value is reused.

              During first expansion, the heuristic value is recomputed and might become
              infinite, for example because the reevaluation uses a stronger heuristic or
              because the heuristic is path-dependent and we have accumulated more
              information in the meantime. Then upon second expansion we have a dead-end
              node which we must ignore.
            */
            if (node->is_dead_end())
                continue;

            if (lazy_evaluator->is_estimate_cached(s)) {
                int old_h = lazy_evaluator->get_cached_estimate(s);
                int new_h = eval_context.get_evaluator_value_or_infinity(lazy_evaluator.get());
                if (open_list->is_dead_end(eval_context)) {
                    node->mark_as_dead_end();
                    statistics.inc_dead_ends();
                    continue;
                }
                if (new_h != old_h) {
                    open_list->insert(eval_context, id);
                    continue;
                }
            }
        }

        node->close();
        assert(!node->is_dead_end());
        update_f_value_statistics(eval_context);
        statistics.inc_expanded();
        break;
    }

    const State &s = node->get_state();
    if (check_goal_and_set_plan(s))
        return SOLVED;

    vector<OperatorID> applicable_ops;
    successor_generator.generate_applicable_ops(s, applicable_ops);

    /*
      TODO: When preferred operators are in use, a preferred operator will be
      considered by the preferred operator queues even when it is pruned.
    */
    pruning_method->prune_operators(s, applicable_ops);

    // This evaluates the expanded state (again) to get preferred ops
    EvaluationContext eval_context(s, node->get_g(), false, &statistics, true);
    ordered_set::OrderedSet<OperatorID> preferred_operators;
    for (const shared_ptr<Evaluator> &preferred_operator_evaluator : preferred_operator_evaluators) {
        collect_preferred_operators(eval_context,
                                    preferred_operator_evaluator.get(),
                                    preferred_operators);
    }

    for (OperatorID op_id : applicable_ops) {
        OperatorProxy op = task_proxy.get_operators()[op_id];
        if ((node->get_real_g() + op.get_cost()) >= bound)
            continue;

        State succ_state = state_registry.get_successor_state(s, op);
        statistics.inc_generated();
        bool is_preferred = preferred_operators.contains(op_id);

        SearchNode succ_node = search_space.get_node(succ_state);

        for (Evaluator *evaluator : path_dependent_evaluators) {
            evaluator->notify_state_transition(s, op_id, succ_state);
        }

        // Previously encountered dead end. Don't re-evaluate.
        if (succ_node.is_dead_end())
            continue;

        if (succ_node.is_new()) {
            // We have not seen this state before.
            // Evaluate and create a new node.

            // Careful: succ_node.get_g() is not available here yet,
            // hence the stupid computation of succ_g.
            // TODO: Make this less fragile.
            int succ_g = node->get_g() + get_adjusted_cost(op);

            EvaluationContext succ_eval_context(
                succ_state, succ_g, is_preferred, &statistics);
            statistics.inc_evaluated_states();

            if (open_list->is_dead_end(succ_eval_context)) {
                if(unsolv_type == UnsolvabilityVerificationType::PROOF ||
                          unsolv_type == UnsolvabilityVerificationType::PROOF_DISCARD) {
                    open_list->store_deadend_info(succ_eval_context);
                }
                succ_node.mark_as_dead_end();
                statistics.inc_dead_ends();
                continue;
            }
            succ_node.open(*node, op, get_adjusted_cost(op));

            open_list->insert(succ_eval_context, succ_state.get_id());
            if (search_progress.check_progress(succ_eval_context)) {
                statistics.print_checkpoint_line(succ_node.get_g());
                reward_progress();
            }
        } else if (succ_node.get_g() > node->get_g() + get_adjusted_cost(op)) {
            // We found a new cheapest path to an open or closed state.
            if (reopen_closed_nodes) {
                if (succ_node.is_closed()) {
                    /*
                      TODO: It would be nice if we had a way to test
                      that reopening is expected behaviour, i.e., exit
                      with an error when this is something where
                      reopening should not occur (e.g. A* with a
                      consistent heuristic).
                    */
                    statistics.inc_reopened();
                }
                succ_node.reopen(*node, op, get_adjusted_cost(op));

                EvaluationContext succ_eval_context(
                    succ_state, succ_node.get_g(), is_preferred, &statistics);

                /*
                  Note: our old code used to retrieve the h value from
                  the search node here. Our new code recomputes it as
                  necessary, thus avoiding the incredible ugliness of
                  the old "set_evaluator_value" approach, which also
                  did not generalize properly to settings with more
                  than one evaluator.

                  Reopening should not happen all that frequently, so
                  the performance impact of this is hopefully not that
                  large. In the medium term, we want the evaluators to
                  remember evaluator values for states themselves if
                  desired by the user, so that such recomputations
                  will just involve a look-up by the Evaluator object
                  rather than a recomputation of the evaluator value
                  from scratch.
                */
                open_list->insert(succ_eval_context, succ_state.get_id());
            } else {
                // If we do not reopen closed nodes, we just update the parent pointers.
                // Note that this could cause an incompatibility between
                // the g-value and the actual path that is traced back.
                succ_node.update_parent(*node, op, get_adjusted_cost(op));
            }
        }
    }

    return IN_PROGRESS;
}

void EagerSearch::reward_progress() {
    // Boost the "preferred operator" open lists somewhat whenever
    // one of the heuristics finds a state with a new best h value.
    open_list->boost_preferred();
}

void EagerSearch::dump_search_space() const {
    search_space.dump(task_proxy);
}

void EagerSearch::start_f_value_statistics(EvaluationContext &eval_context) {
    if (f_evaluator) {
        int f_value = eval_context.get_evaluator_value(f_evaluator.get());
        statistics.report_f_value_progress(f_value);
    }
}

/* TODO: HACK! This is very inefficient for simply looking up an h value.
   Also, if h values are not saved it would recompute h for each and every state. */
void EagerSearch::update_f_value_statistics(EvaluationContext &eval_context) {
    if (f_evaluator) {
        int f_value = eval_context.get_evaluator_value(f_evaluator.get());
        statistics.report_f_value_progress(f_value);
    }
}

void add_options_to_parser(OptionParser &parser) {
    SearchEngine::add_pruning_option(parser);
    SearchEngine::add_options_to_parser(parser);
}

void EagerSearch::write_unsolvability_proof() {
    double writing_start = utils::g_timer();

    UnsolvabilityManager unsolvmgr(unsolvability_directory, task);
    std::ofstream &certstream = unsolvmgr.get_stream();
    std::vector<int> varorder(task_proxy.get_variables().size());
    for(size_t i = 0; i < varorder.size(); ++i) {
        varorder[i] = i;
    }

    /*
      TODO: asking if the initial node is new seems wrong, but that is
      how the search handles a dead initial state
     */
    if(search_space.get_node(state_registry.get_initial_state()).is_new()) {
        const State &init_state = state_registry.get_initial_state();
        EvaluationContext eval_context(init_state,
                                       0,
                                       false, &statistics);
        std::pair<int,Judgment> deadend =
                open_list->get_setid_and_deadjudgment(eval_context, unsolvmgr);
        int deadend_set = deadend.first;
        Judgment deadend_set_dead = deadend.second;

        Judgment init_subset_deadend_set = unsolvmgr.make_statement(unsolvmgr.get_initsetid(), deadend_set, "b1");
        Judgment init_dead = unsolvmgr.apply_rule_sd(unsolvmgr.get_initsetid(), init_subset_deadend_set, deadend_set_dead);
        unsolvmgr.apply_rule_ci(init_dead);
        open_list->finish_unsolvability_proof();

        /*
          Writing the task file at the end minimizes the chances that both task and
          proof file are there but the planner could not finish writing them.
         */
        write_unsolvability_task_file(varorder);

        double writing_end = utils::g_timer();
        std::cout << "Time for writing unsolvability proof: "
                  << writing_end - writing_start << std::endl;
        return;
    }

    struct MergeTreeEntry {
        int de_pos_begin;
        int setid;
        Judgment set_dead;
        int depth;
    };

    CuddManager manager(task);
    std::vector<StateID> dead_ends;
    int dead_end_amount = statistics.get_dead_ends();
    dead_ends.reserve(dead_end_amount);

    std::vector<MergeTreeEntry> merge_tree;
    if (dead_end_amount > 0) {
        merge_tree.resize(ceil(log2(dead_end_amount+1)));
    }
    // mt_pos is the index of the first unused entry of merge_tree
    int mt_pos = 0;

    CuddBDD expanded = CuddBDD(&manager, false);
    CuddBDD dead = CuddBDD(&manager, false);

    int fact_amount = 0;
    for(size_t i = 0; i < varorder.size(); ++i) {
        fact_amount += task_proxy.get_variables()[varorder[i]].get_domain_size();
    }

    for(const StateID id : state_registry) {
        const State &state = state_registry.lookup_state(id);
        CuddBDD statebdd = CuddBDD(&manager, state);
        if (search_space.get_node(state).is_dead_end()) {

            dead.lor(statebdd);
            dead_ends.push_back(id);

            EvaluationContext eval_context(state,
                                           0,
                                           false, &statistics);
            std::pair<int,Judgment> deadend =
                    open_list->get_setid_and_deadjudgment(eval_context, unsolvmgr);            
            int deadend_set = deadend.first;
            Judgment deadend_set_dead = deadend.second;


            // prove that an explicit set only containing dead end is dead
            int expl_state_setid = unsolvmgr.get_new_setid();
            certstream << "e " << expl_state_setid << " e ";
            certstream << fact_amount << " ";
            for (int i = 0; i < fact_amount; ++i) {
                certstream << i << " ";
            }
            certstream << ": ";
            unsolvmgr.dump_state(state);
            certstream << " ;\n";
            Judgment state_subset_deadend_set = unsolvmgr.make_statement(expl_state_setid, deadend_set, "b4");
            Judgment state_dead = unsolvmgr.apply_rule_sd(expl_state_setid, state_subset_deadend_set, deadend_set_dead);
            merge_tree[mt_pos].de_pos_begin = dead_ends.size()-1;
            merge_tree[mt_pos].setid = expl_state_setid;
            merge_tree[mt_pos].set_dead = state_dead;
            merge_tree[mt_pos].depth = 0;
            mt_pos++;

            // merge the last 2 sets to a new one if they have the same depth in the merge tree
            while(mt_pos > 1 && merge_tree[mt_pos-1].depth == merge_tree[mt_pos-2].depth) {
                MergeTreeEntry &mte_left = merge_tree[mt_pos-2];
                MergeTreeEntry &mte_right = merge_tree[mt_pos-1];

                // show that implicit union between the two sets is dead
                int implicit_union = unsolvmgr.get_new_setid();
                certstream << "e " << implicit_union << " u "
                           << mte_left.setid << " " << mte_right.setid << "\n";
                Judgment implicit_union_dead = unsolvmgr.apply_rule_ud(implicit_union, mte_left.set_dead, mte_right.set_dead);

                // the left entry represents the merged entry while the right entry will be considered deleted
                mte_left.depth++;
                mte_left.setid = implicit_union;
                mte_left.set_dead = implicit_union_dead;
                mt_pos--;
            }

        } else if(search_space.get_node(state).is_closed()) {
            expanded.lor(statebdd);
        }
    }

    std::vector<CuddBDD> bdds;
    std::string filename_search_bdds = unsolvmgr.get_directory() + "search.bdd";
    int de_setid;
    Judgment deadends_dead;

    // no dead ends --> use empty set
    if(dead_ends.size() == 0) {
        de_setid = unsolvmgr.get_emptysetid();
        deadends_dead = unsolvmgr.apply_rule_ed();
    } else {
        // if the merge tree is not a complete binary tree, we first need to shrink it up to size 1
        // TODO: this is copy paste from above...
        while(mt_pos > 1) {
            MergeTreeEntry &mte_left = merge_tree[mt_pos-2];
            MergeTreeEntry &mte_right = merge_tree[mt_pos-1];

            // show that implicit union between the two sets is dead
            int implicit_union = unsolvmgr.get_new_setid();
            certstream << "e " << implicit_union << " u "
                       << mte_left.setid << " " << mte_right.setid << "\n";
            Judgment implicit_union_dead = unsolvmgr.apply_rule_ud(implicit_union, mte_left.set_dead, mte_right.set_dead);
            mt_pos--;
            merge_tree[mt_pos-1].depth++;
            merge_tree[mt_pos-1].setid = implicit_union;
            merge_tree[mt_pos-1].set_dead = implicit_union_dead;
        }
        bdds.push_back(dead);

        // build an explicit set containing all dead ends
        int all_de_explicit = unsolvmgr.get_new_setid();
        certstream << "e " << all_de_explicit << " e ";
        certstream << fact_amount << " ";
        for (int i = 0; i < fact_amount; ++i) {
            certstream << i << " ";
        }
        certstream << ": ";
        for(const StateID id : state_registry) {
            const State &state = state_registry.lookup_state(id);
            if (search_space.get_node(state).is_dead_end()) {
                unsolvmgr.dump_state(state);
                certstream << " ";
            }
        }
        certstream << ";\n";

        // show that all_de_explicit is a subset to the union of all dead ends and thus dead
        Judgment expl_deadends_subset = unsolvmgr.make_statement(all_de_explicit, merge_tree[0].setid, "b1");
        Judgment expl_deadends_dead = unsolvmgr.apply_rule_sd(all_de_explicit, expl_deadends_subset, merge_tree[0].set_dead);

        // show that the bdd containing all dead ends is a subset to the explicit set containing all dead ends
        int bdd_dead_setid = unsolvmgr.get_new_setid();
        certstream << "e " << bdd_dead_setid << " b " << filename_search_bdds
                   << " " << bdds.size()-1 << " ;\n";

        Judgment bdd_subset_explicit = unsolvmgr.make_statement(bdd_dead_setid, all_de_explicit, "b4");
        Judgment bdd_dead = unsolvmgr.apply_rule_sd(bdd_dead_setid, bdd_subset_explicit, expl_deadends_dead);

        de_setid = bdd_dead_setid;
        deadends_dead = bdd_dead;
    }

    bdds.push_back(expanded);

    // show that expanded states only lead to themselves and dead states
    int expanded_setid = unsolvmgr.get_new_setid();
    certstream << "e " << expanded_setid << " b " << filename_search_bdds << " "
               << bdds.size()-1 << " ;\n";
    int expanded_progression_setid = unsolvmgr.get_new_setid();
    certstream << "e " << expanded_progression_setid << " p " << expanded_setid << " 0\n";
    int union_expanded_dead = unsolvmgr.get_new_setid();
    certstream << "e " << union_expanded_dead << " u "
               << expanded_setid << " " << de_setid << "\n";
    int intersection_expanded_goal = unsolvmgr.get_new_setid();
    certstream << "e " << intersection_expanded_goal << " i "
               << expanded_setid << " " << unsolvmgr.get_goalsetid() << "\n";

    Judgment empty_dead = unsolvmgr.apply_rule_ed();
    Judgment progression_to_deadends = unsolvmgr.make_statement(expanded_progression_setid, union_expanded_dead, "b2");
    Judgment goal_intersection_empty = unsolvmgr.make_statement(intersection_expanded_goal, unsolvmgr.get_emptysetid(), "b1");
    Judgment goal_intersection_dead = unsolvmgr.apply_rule_sd(intersection_expanded_goal, goal_intersection_empty, empty_dead);
    Judgment expanded_dead = unsolvmgr.apply_rule_pg(expanded_setid, progression_to_deadends, deadends_dead, goal_intersection_dead);

    Judgment init_in_expanded = unsolvmgr.make_statement(unsolvmgr.get_initsetid(), expanded_setid, "b1");
    Judgment init_dead = unsolvmgr.apply_rule_sd(unsolvmgr.get_initsetid(), init_in_expanded, expanded_dead);
    unsolvmgr.apply_rule_ci(init_dead);

    open_list->finish_unsolvability_proof();

    manager.dumpBDDs(bdds, filename_search_bdds);

    /*
      Writing the task file at the end minimizes the chances that both task and
      proof file are there but the planner could not finish writing them.
     */
    write_unsolvability_task_file(varorder);

    double writing_end = utils::g_timer();
    std::cout << "Time for writing unsolvability proof: "
              << writing_end - writing_start << std::endl;
}


void EagerSearch::write_unsolvability_task_file(const std::vector<int> &varorder) {
    assert(varorder.size() == task_proxy.get_variables().size());
    std::vector<std::vector<int>> fact_to_var(varorder.size(), std::vector<int>());
    int fact_amount = 0;
    for(size_t i = 0; i < varorder.size(); ++i) {
        int var = varorder[i];
        fact_to_var[var].resize(task_proxy.get_variables()[var].get_domain_size());
        for(int j = 0; j < task_proxy.get_variables()[var].get_domain_size(); ++j) {
            fact_to_var[var][j] = fact_amount++;
        }
    }

    std::ofstream task_file;
    task_file.open("task.txt");

    task_file << "begin_atoms:" << fact_amount << "\n";
    for(size_t i = 0; i < varorder.size(); ++i) {
        int var = varorder[i];
        for(int j = 0; j < task_proxy.get_variables()[var].get_domain_size(); ++j) {
            task_file << task_proxy.get_variables()[var].get_fact(j).get_name() << "\n";
        }
    }
    task_file << "end_atoms\n";

    task_file << "begin_init\n";
    for(size_t i = 0; i < task_proxy.get_variables().size(); ++i) {
        task_file << fact_to_var[i][task_proxy.get_initial_state()[i].get_value()] << "\n";
    }
    task_file << "end_init\n";

    task_file << "begin_goal\n";
    for(size_t i = 0; i < task_proxy.get_goals().size(); ++i) {
        FactProxy f = task_proxy.get_goals()[i];
        task_file << fact_to_var[f.get_variable().get_id()][f.get_value()] << "\n";
    }
    task_file << "end_goal\n";


    task_file << "begin_actions:" << task_proxy.get_operators().size() << "\n";
    for(size_t op_index = 0;  op_index < task_proxy.get_operators().size(); ++op_index) {
        OperatorProxy op = task_proxy.get_operators()[op_index];

        task_file << "begin_action\n"
                  << op.get_name() << "\n"
                  << "cost: "<< op.get_cost() <<"\n";
        PreconditionsProxy pre = op.get_preconditions();
        EffectsProxy post = op.get_effects();

        for(size_t i = 0; i < pre.size(); ++i) {
            task_file << "PRE:" << fact_to_var[pre[i].get_variable().get_id()][pre[i].get_value()] << "\n";
        }
        for(size_t i = 0; i < post.size(); ++i) {
            if(!post[i].get_conditions().empty()) {
                std::cout << "CONDITIONAL EFFECTS, ABORT!";
                task_file.close();
                std::remove("task.txt");
                utils::exit_with(utils::ExitCode::SEARCH_CRITICAL_ERROR);
            }
            FactProxy f = post[i].get_fact();
            task_file << "ADD:" << fact_to_var[f.get_variable().get_id()][f.get_value()] << "\n";
            // all other facts from this FDR variable are set to false
            // TODO: can we make this more compact / smarter?
            for(int j = 0; j < f.get_variable().get_domain_size(); j++) {
                if(j == f.get_value()) {
                    continue;
                }
                task_file << "DEL:" << fact_to_var[f.get_variable().get_id()][j] << "\n";
            }
        }
        task_file << "end_action\n";
    }
    task_file << "end_actions\n";
    task_file.close();
}

}
