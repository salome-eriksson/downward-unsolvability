#include "max_heuristic.h"

#include "../plugins/plugin.h"
#include "../utils/logging.h"

#include <cassert>
#include <vector>

using namespace std;

namespace max_heuristic {
/*
  TODO: At the time of this writing, this shares huge amounts of code
        with h^add, and the two should be refactored so that the
        common code is only included once, in so far as this is
        possible without sacrificing run-time. We may want to avoid
        virtual calls in the inner-most loops; maybe a templated
        strategy pattern is an option. Right now, the only differences
        to the h^add code are the use of max() instead of add() and
        the lack of preferred operator support (but we might actually
        reintroduce that if it doesn't hurt performance too much).
 */

// construction and destruction
HSPMaxHeuristic::HSPMaxHeuristic(const plugins::Options &opts)
    : RelaxationHeuristic(opts) {
    unsolv_subsumption_check = opts.get<bool>("unsolv_subsumption");
    if (log.is_at_least_normal()) {
        log << "Initializing HSP max heuristic..." << endl;
    }
}

// heuristic computation
void HSPMaxHeuristic::setup_exploration_queue() {
    queue.clear();

    for (Proposition &prop : propositions)
        prop.cost = -1;

    // Deal with operators and axioms without preconditions.
    for (UnaryOperator &op : unary_operators) {
        op.unsatisfied_preconditions = op.num_preconditions;
        op.cost = op.base_cost; // will be increased by precondition costs

        if (op.unsatisfied_preconditions == 0)
            enqueue_if_necessary(op.effect, op.base_cost);
    }
}

void HSPMaxHeuristic::setup_exploration_queue_state(const State &state) {
    for (FactProxy fact : state) {
        PropID init_prop = get_prop_id(fact);
        enqueue_if_necessary(init_prop, 0);
    }
}

void HSPMaxHeuristic::relaxed_exploration() {
    int unsolved_goals = goal_propositions.size();
    while (!queue.empty()) {
        pair<int, PropID> top_pair = queue.pop();
        int distance = top_pair.first;
        PropID prop_id = top_pair.second;
        Proposition *prop = get_proposition(prop_id);
        int prop_cost = prop->cost;
        assert(prop_cost >= 0);
        assert(prop_cost <= distance);
        if (prop_cost < distance)
            continue;
        if (prop->is_goal && --unsolved_goals == 0)
            return;
        for (OpID op_id : precondition_of_pool.get_slice(
                 prop->precondition_of, prop->num_precondition_occurences)) {
            UnaryOperator *unary_op = get_operator(op_id);
            unary_op->cost = max(unary_op->cost,
                                 unary_op->base_cost + prop_cost);
            --unary_op->unsatisfied_preconditions;
            assert(unary_op->unsatisfied_preconditions >= 0);
            if (unary_op->unsatisfied_preconditions == 0)
                enqueue_if_necessary(unary_op->effect, unary_op->cost);
        }
    }
}

int HSPMaxHeuristic::compute_heuristic(const State &ancestor_state) {
    State state = convert_ancestor_state(ancestor_state);

    setup_exploration_queue();
    setup_exploration_queue_state(state);
    relaxed_exploration();

    int total_cost = 0;
    for (PropID goal_id : goal_propositions) {
        const Proposition *goal = get_proposition(goal_id);
        int goal_cost = goal->cost;
        if (goal_cost == -1)
            return DEAD_END;
        total_cost = max(total_cost, goal_cost);
    }
    return total_cost;
}

std::pair<SetExpression, Judgment> HSPMaxHeuristic::justify_h_value (
        CertificateManager &certmgr, State &state) {
    // NOTE: the commented out code is an alternative representation with horn formulas
    /*std::vector<std::vector<int>> clauses(0);
    static SetExpression allstates = certmgr.define_horn_formula(0, clauses);*/

    // TODO: don't (ab)use unsolvability_setup flag
    if(!unsolvability_setup) {
        cudd_manager = new CuddManager(task);
        unsolvability_setup = true;
    }
    static SetExpression allstates = certmgr.define_bdd(CuddBDD(cudd_manager, true));
    static Judgment allstates_bound = certmgr.apply_rule_tc(allstates);
    static Judgment empty_bound = certmgr.apply_rule_ec();
    static Judgment allactions_subset_allactions = certmgr.make_statement(certmgr.get_allactions(), certmgr.get_allactions(), "b5");

    int h_value = compute_heuristic(state);

    if (h_value == DEAD_END) {
        std::vector<std::pair<int,int>> unreachable_facts;
//        std::unordered_set<int>unreachable_vars(0);
        for (size_t var = 0; var < task_proxy.get_variables().size(); ++var) {
            for (size_t val = 0; val < (size_t) task_proxy.get_variables()[var].get_domain_size(); ++val) {
                Proposition *prop = get_proposition(var,val);
                if (prop->cost == -1) {
                    unreachable_facts.push_back({var,val});
//                    unreachable_vars.insert(certmgr.get_variable(var,val));
                }
            }
        }
/*        for (int var: unreachable_vars) {
            clauses.push_back({-(var+1)}); //Dimacs needs an offset of 1
        }*/
        SetExpression formula = certmgr.define_bdd(CuddBDD(cudd_manager, {}, unreachable_facts));
//        SetExpression formula = certmgr.define_horn_formula(certmgr.get_factamount(), clauses);
        SetExpression intersection = certmgr.define_set_intersection(formula, certmgr.get_goalset());
        Judgment goal_intersection_empty = certmgr.make_statement(intersection, certmgr.get_emptyset(), "b1");
        SetExpression progression = certmgr.define_set_progression(formula, certmgr.get_allactions());
        SetExpression union_with_empty = certmgr.define_set_union(formula, certmgr.get_emptyset());
        Judgment prog_judgment = certmgr.make_statement(progression, union_with_empty, "b2");
        Judgment infinite_bound = certmgr.apply_rule_pc(formula, std::numeric_limits<unsigned>::max(),
                                                        allactions_subset_allactions, goal_intersection_empty,
                                                        {{prog_judgment, empty_bound}});
        return {formula, infinite_bound};
    } else {
        std::unordered_map<int,std::vector<std::pair<int,int>>> vars_by_bound(0);
        for (size_t var = 0; var < task_proxy.get_variables().size(); ++var) {
            for (int val = 0; val < task_proxy.get_variables()[var].get_domain_size(); ++val) {
                Proposition *prop = get_proposition(var,val);
                int cost = (prop->cost == -1) ? h_value+1 : prop->cost;
                if (cost > 0) {
                    vars_by_bound[std::max(0,h_value-cost)].push_back({var,val});
                    //vars_by_bound[std::max(0,h_value-cost)].push_back(certmgr.get_variable(var,val));
                }
            }
        }

        std::vector<int> bounds;
        bounds.reserve(vars_by_bound.size());
        for (auto it : vars_by_bound) {
            bounds.push_back(it.first);
        }
        bounds.push_back(h_value);
        std::sort(bounds.begin(), bounds.end());

        std::vector<std::pair<int,int>> negative_facts;
        std::vector<std::pair<SetExpression,Judgment>> sets_and_their_bounds;
        sets_and_their_bounds.push_back({allstates,allstates_bound});
        for (size_t bound_id = 1; bound_id < bounds.size(); ++bound_id) {
            //prepare new formula
            int bound = bounds[bound_id];
            for (std::pair<int,int> fact : vars_by_bound[bounds[bound_id-1]]) {
//                clauses.push_back({-(var+1)}); //Dimacs needs an offset of 1
                negative_facts.push_back(fact);
            }
//            SetExpression bound_set = certmgr.define_horn_formula(certmgr.get_factamount(), clauses);
            SetExpression bound_set = certmgr.define_bdd(CuddBDD(cudd_manager, {}, negative_facts));

            // Judgments for PC rule
            SetExpression goal_intersection = certmgr.define_set_intersection(bound_set, certmgr.get_goalset());
            Judgment empty_goal = certmgr.make_statement(goal_intersection, certmgr.get_emptyset(), "b1");
            std::vector<std::pair<Judgment,Judgment>> successor_bounds;
            for (auto element : certmgr.get_sorted_actions()) {
                SetExpression actionset = element.first;
                int action_cost = element.second;
                auto it = std::lower_bound(bounds.begin(), bounds.end(), bound-action_cost);
                size_t index = std::distance(bounds.begin(), it);
                std::pair<SetExpression, Judgment> background =
                        (*it == bound) ? std::make_pair(certmgr.get_emptyset(), empty_bound)
                                             : sets_and_their_bounds[index];

                SetExpression progression = certmgr.define_set_progression(bound_set, actionset);
                SetExpression right_union = certmgr.define_set_union(bound_set, background.first);
                Judgment prog_judgment = certmgr.make_statement(progression, right_union, "b2");
                successor_bounds.push_back({prog_judgment, background.second});
            }
            Judgment bound_judgment = certmgr.apply_rule_pc(bound_set, bound, certmgr.get_all_actions_contained_judgment(),
                                                            empty_goal, successor_bounds);
            sets_and_their_bounds.push_back({bound_set, bound_judgment});
        }
        return sets_and_their_bounds[bounds.size()-1];
    }
}


class HSPMaxHeuristicFeature : public plugins::TypedFeature<Evaluator, HSPMaxHeuristic> {
public:
    HSPMaxHeuristicFeature() : TypedFeature("hmax") {
        document_title("Max heuristic");
        add_option<bool>("unsolv_subsumption",
                         "Check if a dead-end is covered by a previous one."
                         "This is more time efficient for verification, but"
                         "less time efficient for generation.",
                         "false");

        Heuristic::add_options_to_feature(*this);

        document_language_support("action costs", "supported");
        document_language_support("conditional effects", "supported");
        document_language_support(
            "axioms",
            "supported (in the sense that the planner won't complain -- "
            "handling of axioms might be very stupid "
            "and even render the heuristic unsafe)");

        document_property("admissible", "yes for tasks without axioms");
        document_property("consistent", "yes for tasks without axioms");
        document_property("safe", "yes for tasks without axioms");
        document_property("preferred operators", "no");
    }
};

static plugins::FeaturePlugin<HSPMaxHeuristicFeature> _plugin;
}
