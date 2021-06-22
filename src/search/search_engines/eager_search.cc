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
        if(unsolv_type == UnsolvabilityVerificationType::CERTIFICATE ||
                unsolv_type == UnsolvabilityVerificationType::CERTIFICATE_FASTDUMP ||
                unsolv_type == UnsolvabilityVerificationType::CERTIFICATE_NOHINTS) {
            open_list->create_subcertificate(eval_context);
        } else if (unsolv_type == UnsolvabilityVerificationType::PROOF ||
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

    if(unsolv_type == UnsolvabilityVerificationType::CERTIFICATE ||
            unsolv_type == UnsolvabilityVerificationType::CERTIFICATE_FASTDUMP) {
        unsolvability_certificate_hints.open(unsolvability_directory + "hints.txt");
    }
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
            if(unsolv_type == UnsolvabilityVerificationType::CERTIFICATE ||
                    unsolv_type == UnsolvabilityVerificationType::CERTIFICATE_FASTDUMP ||
                    unsolv_type == UnsolvabilityVerificationType::CERTIFICATE_NOHINTS) {
                write_unsolvability_certificate();
            }
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

    if(unsolv_type == UnsolvabilityVerificationType::CERTIFICATE ||
            unsolv_type == UnsolvabilityVerificationType::CERTIFICATE_FASTDUMP) {
        unsolvability_certificate_hints << s.get_id().get_value() << " " << applicable_ops.size();
    }

    for (OperatorID op_id : applicable_ops) {
        OperatorProxy op = task_proxy.get_operators()[op_id];
        if ((node->get_real_g() + op.get_cost()) >= bound) {
            if(unsolv_type == UnsolvabilityVerificationType::CERTIFICATE ||
                    unsolv_type == UnsolvabilityVerificationType::CERTIFICATE_FASTDUMP) {
                unsolvability_certificate_hints << " " <<  op.get_id() << " -1";
            }
            continue;
        }

        State succ_state = state_registry.get_successor_state(s, op);
        statistics.inc_generated();
        bool is_preferred = preferred_operators.contains(op_id);

        SearchNode succ_node = search_space.get_node(succ_state);

        for (Evaluator *evaluator : path_dependent_evaluators) {
            evaluator->notify_state_transition(s, op_id, succ_state);
        }

        // Previously encountered dead end. Don't re-evaluate.
        if (succ_node.is_dead_end()) {
            if(unsolv_type == UnsolvabilityVerificationType::CERTIFICATE ||
                    unsolv_type == UnsolvabilityVerificationType::CERTIFICATE_FASTDUMP) {
                EvaluationContext succ_eval_context(
                    succ_state, succ_node.get_g(), is_preferred, &statistics);
                // TODO: need to call something in order for the state to actually be evaluated, but this might be inefficient
                open_list->is_dead_end(succ_eval_context);
                int hint = open_list->create_subcertificate(succ_eval_context);
                unsolvability_certificate_hints << " " << op.get_id() << " " << hint;
            }
            continue;
        }

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
                if(unsolv_type == UnsolvabilityVerificationType::CERTIFICATE ||
                        unsolv_type == UnsolvabilityVerificationType::CERTIFICATE_FASTDUMP) {
                    int hint = open_list->create_subcertificate(succ_eval_context);
                    unsolvability_certificate_hints << " " << op.get_id() << " " << hint;
                } else if(unsolv_type == UnsolvabilityVerificationType::CERTIFICATE_NOHINTS) {
                    open_list->create_subcertificate(succ_eval_context);
                } else if(unsolv_type == UnsolvabilityVerificationType::PROOF ||
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
        if(unsolv_type == UnsolvabilityVerificationType::CERTIFICATE ||
                unsolv_type == UnsolvabilityVerificationType::CERTIFICATE_FASTDUMP) {
            unsolvability_certificate_hints << " " << op.get_id() << " " << succ_state.get_id().get_value();
        }
    }
    if(unsolv_type == UnsolvabilityVerificationType::CERTIFICATE ||
            unsolv_type == UnsolvabilityVerificationType::CERTIFICATE_FASTDUMP) {
        unsolvability_certificate_hints << "\n";
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

void dump_statebdd(const State &s, std::ofstream &statebdd_file,
                   int amount_vars, const std::vector<std::vector<int>> &fact_to_var) {
    // first dump amount of bdds (=1) and index
    statebdd_file << "1 " << s.get_id().get_value() << "\n";

    // header
    statebdd_file << ".ver DDDMP-2.0\n";
    statebdd_file << ".mode A\n";
    statebdd_file << ".varinfo 0\n";
    statebdd_file << ".nnodes " << amount_vars+1 << "\n";
    statebdd_file << ".nvars " << amount_vars << "\n";
    statebdd_file << ".nsuppvars " << amount_vars << "\n";
    statebdd_file << ".ids";
    for(int i = 0; i < amount_vars; ++i) {
        statebdd_file << " " << i;
    }
    statebdd_file << "\n";
    statebdd_file << ".permids";
    for(int i = 0; i < amount_vars; ++i) {
        statebdd_file << " " << i;
    }
    statebdd_file << "\n";
    statebdd_file << ".nroots 1\n";
    statebdd_file << ".rootids -" << amount_vars+1 << "\n";
    statebdd_file << ".nodes\n";

    // nodes
    // TODO: this is a huge mess because only "false" arcs can be minus
    // We start with a negative root, and if the last var is false, we can
    // put a minus in the last arc and reach true in this way.
    // if the last var is true, we assume that the second last is false (because FDR)
    // and thus put a minus on the second last arc which means the last node is "positive"
    std::vector<bool> state_vars(amount_vars, false);
    for (FactProxy fact : s) {
        state_vars[fact_to_var.at(fact.get_variable().get_id()).at(fact.get_value())] = true;
    }
    assert(!state_vars[amount_vars-1] || !state_vars[amount_vars-2]);
    bool last_true = state_vars[amount_vars-1];
    int var = amount_vars-1;

    statebdd_file << "1 T 1 0 0\n";
    statebdd_file << "2 " << var << " " << var << " 1 -1\n";
    var--;
    statebdd_file << "3 " << var << " " << var << " ";
    if(last_true) {
        statebdd_file << "1 -2\n";
    } else if(state_vars[var]) {
        statebdd_file << "2 1\n";
    } else {
        statebdd_file << "1 2\n";
    }
    var--;

    for(int i = 4; i <= amount_vars+1; ++i) {
        statebdd_file << i << " " << var << " " << var << " ";
        if(state_vars[var]) {
            statebdd_file << i-1 << " 1\n";
        } else {
            statebdd_file << "1 " << i-1 << "\n";
        }
        var--;
    }
    statebdd_file << ".end\n";
}

void EagerSearch::write_unsolvability_certificate() {
    if (unsolv_type == UnsolvabilityVerificationType::CERTIFICATE_NOHINTS) {
        unsolvability_certificate_hints.open(unsolvability_directory + "hints.txt");
    }
    unsolvability_certificate_hints << "end hints";
    unsolvability_certificate_hints.close();

    double writing_start = utils::g_timer();
    std::string hcerts_filename = unsolvability_directory + "h_cert.bdd";
    open_list->write_subcertificates(hcerts_filename);

    std::vector<int> varorder = open_list->get_varorder();
    if(varorder.empty()) {
        varorder.resize(task_proxy.get_variables().size());
        for(size_t i = 0; i < varorder.size(); ++i) {
            varorder[i] = i;
        }
    }
    std::vector<std::vector<int>> fact_to_var(varorder.size(), std::vector<int>());
    int varamount = 0;
    for(size_t i = 0; i < varorder.size(); ++i) {
        int var = varorder[i];
        fact_to_var[var].resize(task_proxy.get_variables()[var].get_domain_size());
        for(int j = 0; j < task_proxy.get_variables()[var].get_domain_size(); ++j) {
            fact_to_var[var][j] = varamount++;
        }
    }

    std::string statebdd_file = unsolvability_directory + "states.bdd";
    if (unsolv_type == UnsolvabilityVerificationType::CERTIFICATE_FASTDUMP) {
        std::ofstream stream;
        stream.open(statebdd_file);
        for(const StateID id : state_registry) {
            // dump bdds of closed states
            const State &state = state_registry.lookup_state(id);
            if(search_space.get_node(state).is_closed()) {
                dump_statebdd(state, stream, varamount, fact_to_var);
            }
        }
        stream.close();
    } else {
        std::vector<CuddBDD> statebdds(0);
        std::vector<int> stateids(0);
        int expanded = statistics.get_expanded();
        CuddManager cudd_manager(task, varorder);
        if (expanded > 0) {
            statebdds.reserve(expanded);
            stateids.reserve(expanded);
            for (const StateID id : state_registry) {
                const State &state = state_registry.lookup_state(id);
                if(search_space.get_node(state).is_closed()) {
                    stateids.push_back(id.get_value());
                    statebdds.push_back(CuddBDD(&cudd_manager, state));
                }
            }
        }
        cudd_manager.dumpBDDs_certificate(statebdds, stateids, statebdd_file);
    }

    // there is currently no safeguard that these are the actual names used
    std::ofstream cert_file;
    cert_file.open(unsolvability_directory + "certificate.txt");
    cert_file << "certificate-type:disjunctive:1\n";
    cert_file << "bdd-files:2\n";
    cert_file << unsolvability_directory << "states.bdd\n";
    cert_file << unsolvability_directory << "h_cert.bdd\n";
    cert_file << "hints:" << unsolvability_directory << "hints.txt\n";
    cert_file.close();

    /*
      Writing the task file at the end minimizes the chances that both task and
      certificate file are there but the planner could not finish writing them.
     */
    write_unsolvability_task_file(varorder);
    double writing_end = utils::g_timer();
    std::cout << "Time for writing unsolvability certificate: " << writing_end - writing_start << std::endl;

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
        std::pair<int,int> dead_superset =
                open_list->get_set_and_deadknowledge_id(eval_context, unsolvmgr);
        int knowledge_init_subset = unsolvmgr.get_new_knowledgeid();
        certstream << "k " << knowledge_init_subset << " s " <<  unsolvmgr.get_initsetid()
                   << " " << dead_superset.first << " b1\n";
        int knowledge_init_dead = unsolvmgr.get_new_knowledgeid();
        certstream << "k " << knowledge_init_dead << " d " << unsolvmgr.get_initsetid()
                   << " sd " << knowledge_init_subset << " " << dead_superset.second << "\n";

        certstream << "k " << unsolvmgr.get_new_knowledgeid() << " u ci "
                   << knowledge_init_dead << "\n";

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
        int k_set_dead;
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
            std::pair<int,int> dead_superset =
                    open_list->get_set_and_deadknowledge_id(eval_context, unsolvmgr);

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
            int k_expl_state_subset = unsolvmgr.get_new_knowledgeid();
            certstream << "k " << k_expl_state_subset << " s " << expl_state_setid << " "
                       << dead_superset.first << " b4\n";
            int k_expl_state_dead = unsolvmgr.get_new_knowledgeid();
            certstream << "k " << k_expl_state_dead << " d " << expl_state_setid
                       << " sd " << k_expl_state_subset << " " << dead_superset.second << "\n";

            merge_tree[mt_pos].de_pos_begin = dead_ends.size()-1;
            merge_tree[mt_pos].setid = expl_state_setid;
            merge_tree[mt_pos].k_set_dead = k_expl_state_dead;
            merge_tree[mt_pos].depth = 0;
            mt_pos++;

            // merge the last 2 sets to a new one if they have the same depth in the merge tree
            while(mt_pos > 1 && merge_tree[mt_pos-1].depth == merge_tree[mt_pos-2].depth) {
                MergeTreeEntry &mte_left = merge_tree[mt_pos-2];
                MergeTreeEntry &mte_right = merge_tree[mt_pos-1];

                // show that implicit union between the two sets is dead
                int impl_union = unsolvmgr.get_new_setid();
                certstream << "e " << impl_union << " u "
                           << mte_left.setid << " " << mte_right.setid << "\n";
                int k_impl_union_dead = unsolvmgr.get_new_knowledgeid();
                certstream << "k " << k_impl_union_dead << " d " << impl_union
                           << " ud " << mte_left.k_set_dead << " " << mte_right.k_set_dead << "\n";


                // build new explicit union and show that it is dead
                /*int union_setid = unsolvmgr.get_new_setid();
                certstream << "e " << union_setid << " e ";
                for(int i = mte_left.de_pos_begin;
                    i < mte_left.de_pos_begin + 2*(1 << mte_left.depth); ++i) {
                    unsolvmgr.dump_state(state_registry.lookup_state(dead_ends[i]));
                    certstream << " ";
                }
                certstream << ";\n";
                int k_union_subset = unsolvmgr.get_new_knowledgeid();
                certstream << "k " << k_union_subset << " s "
                           << union_setid << " " << impl_union << " b1\n";
                int k_union_dead = unsolvmgr.get_new_knowledgeid();
                certstream << "k " << k_union_dead << " d " << union_setid << " sd "
                           << k_union_subset << " " << k_impl_union_dead << "\n";*/

                // the left entry represents the merged entry while the right entry will be considered deleted
                mte_left.depth++;
                mte_left.setid = impl_union;
                mte_left.k_set_dead = k_impl_union_dead;
                mt_pos--;
            }

        } else if(search_space.get_node(state).is_closed()) {
            expanded.lor(statebdd);
        }
    }

    std::vector<CuddBDD> bdds;
    std::string filename_search_bdds = unsolvmgr.get_directory() + "search.bdd";
    int de_setid, k_de_dead;

    // no dead ends --> use empty set
    if(dead_ends.size() == 0) {
        de_setid = unsolvmgr.get_emptysetid();
        k_de_dead = unsolvmgr.get_k_empty_dead();
    } else {
        // if the merge tree is not a complete binary tree, we first need to shrink it up to size 1
        // TODO: this is copy paste from above...
        while(mt_pos > 1) {
            MergeTreeEntry &mte_left = merge_tree[mt_pos-2];
            MergeTreeEntry &mte_right = merge_tree[mt_pos-1];

            // show that implicit union between the two sets is dead
            int impl_union = unsolvmgr.get_new_setid();
            certstream << "e " << impl_union << " u "
                       << mte_left.setid << " " << mte_right.setid << "\n";
            int k_impl_union_dead = unsolvmgr.get_new_knowledgeid();
            certstream << "k " << k_impl_union_dead << " d " << impl_union
                       << " ud " << mte_left.k_set_dead << " " << mte_right.k_set_dead << "\n";

            // build new explicit union and show that it is dead
            /*int union_setid = unsolvmgr.get_new_setid();
            certstream << "e " << union_setid << " e ";
            for(size_t i = mte_left.de_pos_begin; i < dead_ends.size(); ++i) {
                unsolvmgr.dump_state(state_registry.lookup_state(dead_ends[i]));
                certstream << " ";
            }
            certstream << ";\n";
            int k_union_subset = unsolvmgr.get_new_knowledgeid();
            certstream << "k " << k_union_subset << " s "
                       << union_setid << " " << impl_union << " b1\n";
            int k_union_dead = unsolvmgr.get_new_knowledgeid();
            certstream << "k " << k_union_dead << " d " << union_setid << " sd "
                       << k_union_subset << " " << k_impl_union_dead << "\n";*/

            mt_pos--;
            merge_tree[mt_pos-1].depth++;
            merge_tree[mt_pos-1].setid = impl_union;
            merge_tree[mt_pos-1].k_set_dead = k_impl_union_dead;
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
        int k_all_de_explicit_subset = unsolvmgr.get_new_knowledgeid();
        certstream << "k " << k_all_de_explicit_subset << " s "
                   << all_de_explicit << " " << merge_tree[0].setid << " b1\n";
        int k_all_de_explicit_dead = unsolvmgr.get_new_knowledgeid();
        certstream << "k " << k_all_de_explicit_dead << " d " << all_de_explicit
                   << " sd " << k_all_de_explicit_subset << " " << merge_tree[0].k_set_dead << "\n";

        // show that the bdd containing all dead ends is a subset to the explicit set containing all dead ends
        int bdd_dead_setid = unsolvmgr.get_new_setid();
        certstream << "e " << bdd_dead_setid << " b " << filename_search_bdds
                   << " " << bdds.size()-1 << " ;\n";

        int k_bdd_subset_expl = unsolvmgr.get_new_knowledgeid();
        certstream << "k " << k_bdd_subset_expl << " s "
                   << bdd_dead_setid << " " << all_de_explicit << " b4\n";
        int k_bdd_dead = unsolvmgr.get_new_knowledgeid();
        certstream << "k " << k_bdd_dead << " d " << bdd_dead_setid
                   << " sd " << k_bdd_subset_expl << " " << k_all_de_explicit_dead << "\n";

        de_setid = bdd_dead_setid;
        k_de_dead = k_bdd_dead;
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

    int k_progression = unsolvmgr.get_new_knowledgeid();
    certstream << "k " << k_progression << " s "
               << expanded_progression_setid << " " << union_expanded_dead << " b2\n";

    int intersection_expanded_goal = unsolvmgr.get_new_setid();
    certstream << "e " << intersection_expanded_goal << " i "
               << expanded_setid << " " << unsolvmgr.get_goalsetid() << "\n";
    int k_exp_goal_empty = unsolvmgr.get_new_knowledgeid();
    certstream << "k " << k_exp_goal_empty << " s "
               << intersection_expanded_goal << " " << unsolvmgr.get_emptysetid() << " b1\n";
    int k_exp_goal_dead = unsolvmgr.get_new_knowledgeid();
    certstream << "k " << k_exp_goal_dead << " d " << intersection_expanded_goal
               << " sd " << k_exp_goal_empty << " " << unsolvmgr.get_k_empty_dead() << "\n";

    int k_exp_dead = unsolvmgr.get_new_knowledgeid();
    certstream << "k " << k_exp_dead << " d " << expanded_setid << " pg "
               << k_progression << " " << k_de_dead << " " << k_exp_goal_dead << "\n";

    int k_init_in_exp = unsolvmgr.get_new_knowledgeid();
    certstream << "k " << k_init_in_exp << " s "
               << unsolvmgr.get_initsetid() << " " << expanded_setid << " b1\n";
    int k_init_dead = unsolvmgr.get_new_knowledgeid();
    certstream << "k " << k_init_dead << " d " << unsolvmgr.get_initsetid() << " sd "
               << k_init_in_exp << " " << k_exp_dead << "\n";
    certstream << "k " << unsolvmgr.get_new_knowledgeid() << " u ci " << k_init_dead << "\n";

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
