#include "eager_search.h"

#include "../evaluation_context.h"
#include "../evaluator.h"
#include "../open_list_factory.h"
#include "../option_parser.h"
#include "../pruning_method.h"

#include "../algorithms/ordered_set.h"
#include "../task_utils/successor_generator.h"

#include "../utils/logging.h"

#include "../certificates/certificatemanager.h"

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
      open_list(opts.get<shared_ptr<OpenListFactory>>("open")->
                create_state_open_list()),
      f_evaluator(opts.get<shared_ptr<Evaluator>>("f_eval", nullptr)),
      preferred_operator_evaluators(opts.get_list<shared_ptr<Evaluator>>("preferred")),
      lazy_evaluator(opts.get<shared_ptr<Evaluator>>("lazy_evaluator", nullptr)),
      pruning_method(opts.get<shared_ptr<PruningMethod>>("pruning")),
      certificate_directory(opts.get<std::string>("certificate_directory")) {
    if (lazy_evaluator && !lazy_evaluator->does_cache_estimates()) {
        cerr << "lazy_evaluator must cache its estimates" << endl;
        utils::exit_with(utils::ExitCode::SEARCH_INPUT_ERROR);
    }

    if(certificate_directory.compare(".") == 0) {
        certificate_directory = "";
    }
    // expand environment variables
    size_t found = certificate_directory.find('$');
    while(found != std::string::npos) {
        size_t end = certificate_directory.find('/');
        std::string envvar;
        if(end == std::string::npos) {
            envvar = certificate_directory.substr(found+1);
        } else {
            envvar = certificate_directory.substr(found+1,end-found-1);
        }
        // to upper case
        for(size_t i = 0; i < envvar.size(); i++) {
            envvar.at(i) = toupper(envvar.at(i));
        }
        std::string expanded = std::getenv(envvar.c_str());
        certificate_directory.replace(found,envvar.length()+1,expanded);
        found = certificate_directory.find('$');
    }
    if(!certificate_directory.empty() && !(certificate_directory.back() == '/')) {
        certificate_directory += "/";
    }
    std::cout << "Generating unsolvability verification in "
              << certificate_directory << std::endl;
    CuddManager::set_compact_proof(true);
}

void EagerSearch::initialize() {
    log << "Conducting best first search"
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
        log << "Initial state is a dead end." << endl;
    } else {
        if (search_progress.check_progress(eval_context))
            statistics.print_checkpoint_line(0);
        start_f_value_statistics(eval_context);
        SearchNode node = search_space.get_node(initial_state);
        node.open_initial();

        open_list->insert(eval_context, initial_state.get_id());
    }

    print_initial_evaluator_values(eval_context, log);

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
            log << "Completely explored state space -- no solution!" << endl;
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
    if (check_goal_and_set_plan(s)) {
        write_certificate(calculate_plan_cost(get_plan(), task_proxy));
        return SOLVED;
    }

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

void EagerSearch::write_certificate(int optimal_cost) {
    std::cout << "optimal cost: " << optimal_cost << std::endl;
    double writing_start = utils::g_timer();
    CertificateManager certmgr(certificate_directory, task);
    std::vector<int> varorder(task_proxy.get_variables().size());
    for(size_t i = 0; i < varorder.size(); ++i) {
        varorder[i] = i;
    }

    // group action indices according to their cost.
    std::unordered_map<int, std::vector<int>> actions_by_cost;
    for (size_t index = 0; index < task_proxy.get_operators().size(); ++index) {
        OperatorProxy op = task_proxy.get_operators()[index];
        actions_by_cost[op.get_cost()].push_back(index);
    }
    std::vector<int> sorted_action_costs;
    sorted_action_costs.reserve(actions_by_cost.size());
    for (auto& it : actions_by_cost) {
        sorted_action_costs.push_back(it.first);
    }
    std::sort (sorted_action_costs.begin(), sorted_action_costs.end());
    std::vector<SetExpression> sorted_actions;

    for (int cost : sorted_action_costs) {
        sorted_actions.push_back(certmgr.define_action(actions_by_cost[cost]));
    }
    SetExpression action_union = sorted_actions[0];
    for (size_t i = 1; i < sorted_actions.size(); ++i) {
        action_union = certmgr.define_action_set_union(action_union, sorted_actions[i]);
    }
    Judgment all_actions_contained = certmgr.make_statement(certmgr.get_allactions(), action_union, "B5");

    // build BDDs representing all states with a specific g value
    std::unordered_map<int, CuddBDD> states_by_g_value;
    CuddManager manager(task);
    for (StateID id : state_registry) {
        State s = state_registry.lookup_state(id);
        SearchNode node = search_space.get_node(s);
        int g_value = node.get_g();
        if (g_value < optimal_cost) {
            CuddBDD statebdd(&manager, s);
            auto res = states_by_g_value.insert({g_value, statebdd});
            if (!res.second) {
                states_by_g_value[g_value].lor(statebdd);
            }
        }
    }

    std::vector<int> sorted_bounds;
    sorted_bounds.reserve(states_by_g_value.size()+1);
    for (auto& it : states_by_g_value) {
        sorted_bounds.push_back(optimal_cost - it.first);
    }
    sorted_bounds.push_back(0);
    std::sort (sorted_bounds.begin(), sorted_bounds.end());
    std::cout << "Bounds: ";
    for(int bound : sorted_bounds) {
        std::cout << bound << " ";
    }std::cout << std::endl;


    /*
     * Create sets S_i, starting with S_optimalcost = states_by_g_value[0]
     * and iteratively building S_i-1 = S_i + states_by_g_value[optimalcost-i].
     */
    std::vector<SetExpression> bound_sets(sorted_bounds.size());
    CuddBDD lastBDD(&manager, false);
    for (size_t i = sorted_bounds.size()-1; i > 0; --i) {
        std::cout << "sorted bound: " << i << std::endl;
        CuddBDD &bdd = states_by_g_value[optimal_cost-sorted_bounds[i]];
        bdd.lor(lastBDD);
        lastBDD = bdd;
        bound_sets[i] = certmgr.define_bdd(bdd);
    }
    bound_sets[0] = certmgr.define_bdd(CuddBDD(&manager, true));

    std::vector<Judgment> bound_judgments(sorted_bounds.size());
    bound_judgments[0] = certmgr.apply_rule_tc(bound_sets[0]);

    Judgment empty_bound = certmgr.apply_rule_ec();

    for (size_t si = 1; si < sorted_bounds.size(); ++si) {
        int bound = sorted_bounds[si];
        const SetExpression &set = bound_sets[si];
        SetExpression goal_intersection = certmgr.define_set_intersection(set, certmgr.get_goalset());
        Judgment empty_goal = certmgr.make_statement(goal_intersection, certmgr.get_emptyset(), "B1");

        std::vector<std::pair<Judgment,Judgment>> successor_bounds;
        for (size_t ai = 0 ; ai < sorted_action_costs.size(); ++ai) {
            int action_cost = sorted_action_costs[ai];
            int other_bound_index = std::distance(
                        sorted_bounds.begin(),
                        std::lower_bound(sorted_bounds.begin(), sorted_bounds.end(), bound-action_cost));
            std::cout << "other bound index: " << other_bound_index << std::endl;
            const SetExpression &other_set = (si == other_bound_index) ?
                        certmgr.get_emptyset() : bound_sets[other_bound_index];
            SetExpression set_union = certmgr.define_set_union(set, other_set);
            SetExpression progression = certmgr.define_set_progression(set, sorted_actions[ai]);
            Judgment prog_judgment = certmgr.make_statement(progression, set_union, "B2");
            Judgment succ_bound = (si == other_bound_index) ? empty_bound : bound_judgments[other_bound_index];
            successor_bounds.push_back({prog_judgment, succ_bound});
        }
        bound_judgments[si] = certmgr.apply_rule_pc(set, sorted_bounds[si], all_actions_contained, empty_goal, successor_bounds);
    }

    Judgment init_subset = certmgr.make_statement(certmgr.get_initset(), bound_sets.back(), "B1");
    Judgment init_bound = certmgr.apply_rule_sc(certmgr.get_initset(), optimal_cost, bound_judgments.back(), init_subset);

    /*
      TODO: asking if the initial node is new seems wrong, but that is
      how the search handles a dead initial state
     */
    /*if(search_space.get_node(state_registry.get_initial_state()).is_new()) {
        const State &init_state = state_registry.get_initial_state();
        EvaluationContext eval_context(init_state,
                                       0,
                                       false, &statistics);
        std::pair<SetExpression,Judgment> deadend = open_list->get_dead_end_justification(eval_context, certmgr);
        SetExpression deadend_set = deadend.first;
        Judgment deadend_set_dead = deadend.second;
        SetExpression initial_set = certmgr.get_initset();
        Judgment init_subset_deadend_set = certmgr.make_statement(initial_set, deadend_set, "b1");
        Judgment init_dead = certmgr.apply_rule_sd(initial_set, deadend_set_dead, init_subset_deadend_set);
        certmgr.apply_rule_ci(init_dead);
        
        std::cout << "dumping bdds" << std::endl;
        certmgr.dump_BDDs();*/

        /*
          Writing the task file at the end minimizes the chances that both task and
          proof file are there but the planner could not finish writing them.
         */
/*
        write_certificate_task_file(varorder);

        double writing_end = utils::g_timer();
        std::cout << "Time for writing unsolvability proof: "
                  << writing_end - writing_start << std::endl;
        return;
    }
*/
    /*struct MergeTreeEntry {
        SetExpression set;
        Judgment justification;
        int de_pos_begin;
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

    for(StateID id : state_registry) {
        const State &state = state_registry.lookup_state(id);
        CuddBDD statebdd = CuddBDD(&manager, state);
        if (search_space.get_node(state).is_dead_end()) {

            dead.lor(statebdd);
            dead_ends.push_back(id);

            EvaluationContext eval_context(state,
                                           0,
                                           false, &statistics);
            std::pair<SetExpression,Judgment> deadend =
                    open_list->get_dead_end_justification(eval_context, certmgr);
            SetExpression dead_end_set = deadend.first;
            Judgment deadend_set_dead = deadend.second;


            // prove that an explicit set only containing dead end is dead
            SetExpression state_set = certmgr.define_explicit_set(fact_amount, state_registry, {id});
            Judgment state_subset_dead_end_set = certmgr.make_statement(state_set, dead_end_set, "b4");
            Judgment state_dead = certmgr.apply_rule_sd(state_set, deadend_set_dead, state_subset_dead_end_set);
            merge_tree[mt_pos].set = state_set;
            merge_tree[mt_pos].justification = state_dead;
            merge_tree[mt_pos].de_pos_begin = dead_ends.size()-1;
            merge_tree[mt_pos].depth = 0;
            mt_pos++;

            // merge the last 2 sets to a new one if they have the same depth in the merge tree
            while(mt_pos > 1 && merge_tree[mt_pos-1].depth == merge_tree[mt_pos-2].depth) {
                MergeTreeEntry &mte_left = merge_tree[mt_pos-2];
                MergeTreeEntry &mte_right = merge_tree[mt_pos-1];

                // show that implicit union between the two sets is dead
                SetExpression implicit_union = certmgr.define_set_union(mte_left.set, mte_right.set);
                Judgment implicit_union_dead = certmgr.apply_rule_ud(implicit_union, mte_left.justification, mte_right.justification);

                // the left entry represents the merged entry while the right entry will be considered deleted
                mte_left.depth++;
                mte_left.set = implicit_union;
                mte_left.justification = implicit_union_dead;
                mt_pos--;
            }

        } else if(search_space.get_node(state).is_closed()) {
            expanded.lor(statebdd);
        }
        // TODO: this point of the code should never be reached, right? (either its a dead-end or closed)
    }

    std::vector<CuddBDD> bdds;
    SetExpression  dead_end_set;
    Judgment deadends_dead;

    // no dead ends --> use empty set
    if(dead_ends.size() == 0) {
        dead_end_set = certmgr.get_emptyset();
        deadends_dead = certmgr.apply_rule_ed();
    } else {
        // if the merge tree is not a complete binary tree, we first need to shrink it up to size 1
        // TODO: this is copy paste from above...
        while(mt_pos > 1) {
            MergeTreeEntry &mte_left = merge_tree[mt_pos-2];
            MergeTreeEntry &mte_right = merge_tree[mt_pos-1];

            // show that implicit union between the two sets is dead
            SetExpression implicit_union = certmgr.define_set_union(mte_left.set, mte_right.set);
            Judgment implicit_union_dead = certmgr.apply_rule_ud(implicit_union, mte_left.justification, mte_right.justification);
            mt_pos--;
            merge_tree[mt_pos-1].depth++;
            merge_tree[mt_pos-1].set = implicit_union;
            merge_tree[mt_pos-1].justification = implicit_union_dead;
        }
        bdds.push_back(dead);

        // build an explicit set containing all dead ends
        SetExpression all_dead_ends = certmgr.define_explicit_set(fact_amount, state_registry, dead_ends);
        // show that all_de_explicit is a subset to the union of all dead ends and thus dead
        Judgment expl_deadends_subset = certmgr.make_statement(all_dead_ends, merge_tree[0].set, "b1");
        Judgment expl_deadends_dead = certmgr.apply_rule_sd(all_dead_ends, merge_tree[0].justification, expl_deadends_subset);
        // show that the bdd containing all dead ends is a subset to the explicit set containing all dead ends
        SetExpression dead_ends_bdd = certmgr.define_bdd(bdds[bdds.size()-1]);
        Judgment bdd_subset_explicit = certmgr.make_statement(dead_ends_bdd, all_dead_ends, "b4");
        Judgment bdd_dead = certmgr.apply_rule_sd(dead_ends_bdd, expl_deadends_dead, bdd_subset_explicit);

        dead_end_set = dead_ends_bdd;
        deadends_dead = bdd_dead;
    }

    bdds.push_back(expanded);

    // show that expanded states only lead to themselves and dead states
    SetExpression expanded_set = certmgr.define_bdd(bdds[bdds.size()-1]);
    SetExpression expanded_progressed = certmgr.define_set_progression(expanded_set, 0);
    SetExpression expanded_union_dead = certmgr.define_set_union(expanded_set, dead_end_set);
    SetExpression goal_set = certmgr.get_goalset();
    SetExpression goal_intersection = certmgr.define_set_intersection(expanded_set, goal_set);

    Judgment empty_dead = certmgr.apply_rule_ed();
    Judgment progression_to_deadends = certmgr.make_statement(expanded_progressed, expanded_union_dead, "b2");
    Judgment goal_intersection_empty = certmgr.make_statement(goal_intersection, certmgr.get_emptyset(), "b1");
    Judgment goal_intersection_dead = certmgr.apply_rule_sd(goal_intersection, empty_dead, goal_intersection_empty);
    Judgment expanded_dead = certmgr.apply_rule_pg(expanded_set, progression_to_deadends, deadends_dead, goal_intersection_dead);

    Judgment init_in_expanded = certmgr.make_statement(certmgr.get_initset(), expanded_set, "b1");
    Judgment init_dead = certmgr.apply_rule_sd(certmgr.get_initset(), expanded_dead, init_in_expanded);
    certmgr.apply_rule_ci(init_dead);

    std::cout << "dumping bdds" << std::endl;

    certmgr.dump_BDDs();

    std::cout << "done dumping bdds" << std::endl;

    /*
      Writing the task file at the end minimizes the chances that both task and
      proof file are there but the planner could not finish writing them.
     */
/*    write_certificate_task_file(varorder);

    double writing_end = utils::g_timer();
    std::cout << "Time for writing unsolvability proof: "
              << writing_end - writing_start << std::endl;
              */
}


void EagerSearch::write_certificate_task_file(const std::vector<int> &varorder) {
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
    task_file.open(certificate_directory + "task.txt");

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
