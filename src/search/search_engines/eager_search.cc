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
#include "../task_utils/task_properties.h"

#include <cassert>
#include <cstdlib>
#include <deque>
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
      h_evaluator(opts.get<shared_ptr<Evaluator>>("eval", nullptr)),
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
    std::cout << "Generating certificate in "
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
        write_certificate((unsigned) calculate_plan_cost(get_plan(), task_proxy));
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

void EagerSearch::write_certificate(unsigned optimal_cost) {
    task_properties::verify_no_axioms(task_proxy);
    task_properties::verify_no_conditional_effects(task_proxy);
    double writing_start = utils::g_timer();
    CertificateManager certmgr(certificate_directory, task);
    std::vector<int> varorder(task_proxy.get_variables().size());
    for(size_t i = 0; i < varorder.size(); ++i) {
        varorder[i] = i;
    }
    int fact_amount = certmgr.get_factamount();

    std::unordered_map<unsigned, CuddBDD> bdds_by_g_value;
    std::unordered_map<unsigned, std::vector<StateID>> states_by_h_value;
    std::unordered_map<unsigned, CuddBDD> bdds_by_h_value;
    std::unordered_map<unsigned, std::pair<SetExpression, Judgment>> h_bound_info;
    CuddManager manager(task);
    for (StateID id : state_registry) {
        State s = state_registry.lookup_state(id);
        SearchNode node = search_space.get_node(s);
        unsigned g_value = (unsigned) node.get_g();
        if (g_value < optimal_cost || node.is_dead_end()) {
            CuddBDD statebdd(&manager, s);
            EvaluationContext eval_context(s);
            unsigned h_value = 0;
            if (eval_context.is_evaluator_value_infinite(h_evaluator.get())) {
                h_value = std::numeric_limits<unsigned>::max();
            } else {
                h_value = eval_context.get_evaluator_value(h_evaluator.get());
            }
            if  (!node.is_closed() || node.is_dead_end()) {
                states_by_h_value[h_value].push_back(id);
                std::pair<SetExpression, Judgment> new_info = h_evaluator->justify_h_value(certmgr, s);
                SetExpression s_set = certmgr.define_explicit_set(fact_amount, state_registry, {id});
                Judgment s_set_subset = certmgr.make_statement(s_set, new_info.first, "b4");
                Judgment s_set_bound = certmgr.apply_rule_sc(s_set, (unsigned) h_value, new_info.second, s_set_subset);
                new_info = {s_set, s_set_bound};

                auto res = bdds_by_h_value.insert({h_value, statebdd});
                if (!res.second) {
                    bdds_by_h_value[h_value].lor(statebdd);
                    std::pair<SetExpression, Judgment> &existing_info = h_bound_info[h_value];
                    existing_info.first =
                            certmgr.define_set_union(existing_info.first, new_info.first);
                    existing_info.second =
                            certmgr.apply_rule_uc(existing_info.first, (unsigned) h_value,
                                                  existing_info.second, new_info.second);
                } else {
                    h_bound_info[h_value] = new_info;
                }
            } else {
                auto res = bdds_by_g_value.insert({g_value, statebdd});
                if (!res.second) {
                    bdds_by_g_value[g_value].lor(statebdd);
                }
            }
        }
    }

    std::vector<unsigned> sorted_h_values;
    sorted_h_values.reserve(states_by_h_value.size());
    for (auto& it : states_by_h_value) {
        sorted_h_values.push_back(it.first);
    }
    std::sort(sorted_h_values.begin(), sorted_h_values.end());
    for (int i = (int) (sorted_h_values.size()-1); i >= 0; --i) {
        unsigned h_value = sorted_h_values[i];
        // build one explicit state set containing all states s with h(s)=h_value
        SetExpression explicit_set =
                certmgr.define_explicit_set(fact_amount, state_registry, states_by_h_value[h_value]);
        // show that the singular explicit set is a subset of the union of all single state sets
        Judgment explicit_subset_union =
                certmgr.make_statement(explicit_set, h_bound_info[h_value].first, "b1");
        // derive cost bound for singular explicit set
        Judgment explicit_set_bound =
                certmgr.apply_rule_sc(explicit_set, (unsigned) h_value, h_bound_info[h_value].second, explicit_subset_union);
        // define bdd containing all states s with h(s) = h_value
        SetExpression bdd_set = certmgr.define_bdd(bdds_by_h_value[h_value]);
        // show that the bdd is a subset of the singular explicit set
        Judgment bdd_subset_explicit = certmgr.make_statement(bdd_set, explicit_set, "b4");
        // derive the bound for the bdd
        Judgment bdd_bound =
                certmgr.apply_rule_sc(bdd_set, (unsigned) h_value, explicit_set_bound, bdd_subset_explicit);

        if ((size_t) i < sorted_h_values.size()-1) {
            unsigned previous_h_value = sorted_h_values[i+1];
            // show that implicit union with previous bdd has same bound
            SetExpression bdd_union =
                    certmgr.define_set_union(bdd_set, h_bound_info[previous_h_value].first);
            Judgment bdd_union_bound =
                    certmgr.apply_rule_uc(bdd_union, (unsigned) h_value, bdd_bound, h_bound_info[previous_h_value].second);
            // build explicit union with the previous bdd -> it now represents all states with h(s) >= h_value
            bdds_by_h_value[h_value].lor(bdds_by_h_value[previous_h_value]);
            SetExpression new_bdd = certmgr.define_bdd(bdds_by_h_value[h_value]);
            // show that explicit union has the same bound
            Judgment new_bdd_subset =
                    certmgr.make_statement(new_bdd, bdd_union, "b1");
            Judgment new_bdd_bound = certmgr.apply_rule_sc(new_bdd, (unsigned) h_value, bdd_union_bound, new_bdd_subset);
            h_bound_info[h_value] = {new_bdd, new_bdd_bound};
        } else {
            h_bound_info[h_value] = {bdd_set, bdd_bound};
        }
    }
    if (h_bound_info.count(std::numeric_limits<unsigned>::max()) == 0) {
        Judgment empty_bound = certmgr.apply_rule_ec();
        h_bound_info[std::numeric_limits<unsigned>::max()] = {certmgr.get_emptyset(), empty_bound};
        sorted_h_values.push_back(std::numeric_limits<unsigned>::max());
    }

    /*
     * Create sets G_i, starting with G_optimalcost = states_by_g_value[0]
     * and iteratively building G_i+j = G_i + states_by_g_value[optimalcost-(i+j)].
     */
    std::vector<unsigned> sorted_g_bounds;
    sorted_g_bounds.reserve(bdds_by_g_value.size());
    for (auto& it : bdds_by_g_value) {
        sorted_g_bounds.push_back(optimal_cost - it.first);
    }
    std::sort (sorted_g_bounds.begin(), sorted_g_bounds.end());
    std::unordered_map<unsigned, std::pair<SetExpression, Judgment>> g_bound_info;
    CuddBDD lastBDD(&manager, false);
    for (int i = (int) sorted_g_bounds.size()-1; i >= 0; --i) {
        CuddBDD &bdd = bdds_by_g_value[optimal_cost-sorted_g_bounds[i]];
        bdd.lor(lastBDD);
        lastBDD = bdd;
        // TODO: really bad style to use a randm
        g_bound_info[sorted_g_bounds[i]] =
            {certmgr.define_bdd(bdd), certmgr.get_all_actions_contained_judgment()};
    }

    std::unordered_map<unsigned, std::pair<SetExpression, Judgment>> union_bound_info;
    SetExpression trivial_bdd = certmgr.define_bdd(CuddBDD(&manager, true));
    union_bound_info[0] = {trivial_bdd, certmgr.apply_rule_tc(trivial_bdd)};

    for (size_t si = 0; si < sorted_g_bounds.size(); ++si) {
        unsigned bound = sorted_g_bounds[si];
        SetExpression set = g_bound_info[bound].first;
        SetExpression goal_intersection =
                certmgr.define_set_intersection(set, certmgr.get_goalset());
        Judgment empty_goal =
                certmgr.make_statement(goal_intersection, certmgr.get_emptyset(), "b1");

        std::vector<std::pair<Judgment,Judgment>> successor_bounds;
        for (std::pair<SetExpression,int> actionset_and_cost : certmgr.get_sorted_actions()) {
            SetExpression actionset = actionset_and_cost.first;
            unsigned action_cost = (unsigned) actionset_and_cost.second;
            size_t other_bound = (action_cost > bound) ? 0 : bound-action_cost;
            std::pair<SetExpression,Judgment> other_bound_info;
            if (union_bound_info.count(other_bound) > 0) {
                other_bound_info = union_bound_info[other_bound];
            } else {
                unsigned h_bound = *std::lower_bound(
                            sorted_h_values.begin(), sorted_h_values.end(), other_bound);
                unsigned g_bound = *std::lower_bound(
                            sorted_g_bounds.begin(), sorted_g_bounds.end(), other_bound);
                unsigned lower_bound = std::min(h_bound, g_bound);
                if (g_bound == bound) {
                    other_bound_info = h_bound_info[h_bound];
                } else if (union_bound_info.count(lower_bound) > 0) {
                    other_bound_info = union_bound_info[lower_bound];
                } else {
                    SetExpression other_bound_union = certmgr.define_set_union(
                                g_bound_info[g_bound].first,
                                h_bound_info[h_bound].first);
                    Judgment other_bound_union_judgment = certmgr.apply_rule_uc(
                                other_bound_union, lower_bound,
                                g_bound_info[g_bound].second, h_bound_info[h_bound].second);
                    union_bound_info[lower_bound] = {other_bound_union, other_bound_union_judgment};
                    other_bound_info = union_bound_info[lower_bound];
                }
            }
            SetExpression set_union = certmgr.define_set_union(set, other_bound_info.first);
            SetExpression progression = certmgr.define_set_progression(set, actionset);
            Judgment prog_judgment = certmgr.make_statement(progression, set_union, "b2");
            successor_bounds.push_back({prog_judgment, other_bound_info.second});
        }
        g_bound_info[bound].second = certmgr.apply_rule_pc(set, bound, certmgr.get_all_actions_contained_judgment(),
                                                  empty_goal, successor_bounds);
    }

    Judgment init_subset = certmgr.make_statement(certmgr.get_initset(), g_bound_info[optimal_cost].first, "b1");
    Judgment init_bound = certmgr.apply_rule_sc(certmgr.get_initset(), (unsigned) optimal_cost, g_bound_info[optimal_cost].second, init_subset);
    certmgr.apply_rule_bi(optimal_cost, init_bound);

    certmgr.dump_BDDs();

    write_certificate_task_file(varorder);

    double writing_end = utils::g_timer();
    std::cout << "Time for writing optimality proof: "
              << writing_end - writing_start << std::endl;
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
