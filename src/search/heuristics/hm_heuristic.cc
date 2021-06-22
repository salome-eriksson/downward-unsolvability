#include "hm_heuristic.h"

#include "../option_parser.h"
#include "../plugin.h"

#include "../task_utils/task_properties.h"
#include "../utils/logging.h"

#include <cassert>
#include <limits>
#include <set>
#include <sstream>

using namespace std;

namespace hm_heuristic {
HMHeuristic::HMHeuristic(const Options &opts)
    : Heuristic(opts),
      m(opts.get<int>("m")),
      has_cond_effects(task_properties::has_conditional_effects(task_proxy)),
      goals(task_properties::get_fact_pairs(task_proxy.get_goals())),
      unsolvability_setup(false) {
    utils::g_log << "Using h^" << m << "." << endl;
    utils::g_log << "The implementation of the h^m heuristic is preliminary." << endl
                 << "It is SLOOOOOOOOOOOW." << endl
                 << "Please do not use this for comparison!" << endl;
    generate_all_tuples();
}


bool HMHeuristic::dead_ends_are_reliable() const {
    return !task_properties::has_axioms(task_proxy) && !has_cond_effects;
}


int HMHeuristic::compute_heuristic(const State &ancestor_state) {
    State state = convert_ancestor_state(ancestor_state);
    if (task_properties::is_goal_state(task_proxy, state)) {
        return 0;
    } else {
        Tuple s_tup = task_properties::get_fact_pairs(state);

        init_hm_table(s_tup);
        update_hm_table();

        int h = eval(goals);

        if (h == numeric_limits<int>::max())
            return DEAD_END;
        return h;
    }
}


void HMHeuristic::init_hm_table(const Tuple &t) {
    for (auto &hm_ent : hm_table) {
        const Tuple &tuple = hm_ent.first;
        int h_val = check_tuple_in_tuple(tuple, t);
        hm_table[tuple] = h_val;
    }
}


void HMHeuristic::update_hm_table() {
    int round = 0;
    do {
        ++round;
        was_updated = false;

        for (OperatorProxy op : task_proxy.get_operators()) {
            Tuple pre = get_operator_pre(op);

            int c1 = eval(pre);
            if (c1 != numeric_limits<int>::max()) {
                Tuple eff = get_operator_eff(op);
                vector<Tuple> partial_effs;
                generate_all_partial_tuples(eff, partial_effs);
                for (Tuple &partial_eff : partial_effs) {
                    update_hm_entry(partial_eff, c1 + op.get_cost());

                    int eff_size = partial_eff.size();
                    if (eff_size < m) {
                        extend_tuple(partial_eff, op);
                    }
                }
            }
        }
    } while (was_updated);
}


void HMHeuristic::extend_tuple(const Tuple &t, const OperatorProxy &op) {
    for (auto &hm_ent : hm_table) {
        const Tuple &tuple = hm_ent.first;
        bool contradict = false;
        for (const FactPair &fact : tuple) {
            if (contradict_effect_of(op, fact.var, fact.value)) {
                contradict = true;
                break;
            }
        }
        if (!contradict && (tuple.size() > t.size()) && (check_tuple_in_tuple(t, tuple) == 0)) {
            Tuple pre = get_operator_pre(op);

            Tuple others;
            for (const FactPair &fact : tuple) {
                if (find(t.begin(), t.end(), fact) == t.end()) {
                    others.push_back(fact);
                    if (find(pre.begin(), pre.end(), fact) == pre.end()) {
                        pre.push_back(fact);
                    }
                }
            }

            sort(pre.begin(), pre.end());

            set<int> vars;
            bool is_valid = true;
            for (const FactPair &fact : pre) {
                if (vars.count(fact.var) != 0) {
                    is_valid = false;
                    break;
                }
                vars.insert(fact.var);
            }

            if (is_valid) {
                int c2 = eval(pre);
                if (c2 != numeric_limits<int>::max()) {
                    update_hm_entry(tuple, c2 + op.get_cost());
                }
            }
        }
    }
}


int HMHeuristic::eval(const Tuple &t) const {
    vector<Tuple> partial;
    generate_all_partial_tuples(t, partial);
    int max = 0;
    for (Tuple &tuple : partial) {
        assert(hm_table.count(tuple) == 1);

        int h = hm_table.at(tuple);
        if (h > max) {
            max = h;
        }
    }
    return max;
}


int HMHeuristic::update_hm_entry(const Tuple &t, int val) {
    assert(hm_table.count(t) == 1);
    if (hm_table[t] > val) {
        hm_table[t] = val;
        was_updated = true;
    }
    return val;
}


int HMHeuristic::check_tuple_in_tuple(
    const Tuple &tuple, const Tuple &big_tuple) const {
    for (const FactPair &fact0 : tuple) {
        bool found = false;
        for (auto &fact1 : big_tuple) {
            if (fact0 == fact1) {
                found = true;
                break;
            }
        }
        if (!found) {
            return numeric_limits<int>::max();
        }
    }
    return 0;
}


HMHeuristic::Tuple HMHeuristic::get_operator_pre(const OperatorProxy &op) const {
    Tuple preconditions = task_properties::get_fact_pairs(op.get_preconditions());
    sort(preconditions.begin(), preconditions.end());
    return preconditions;
}


HMHeuristic::Tuple HMHeuristic::get_operator_eff(const OperatorProxy &op) const {
    Tuple effects;
    for (EffectProxy eff : op.get_effects()) {
        effects.push_back(eff.get_fact().get_pair());
    }
    sort(effects.begin(), effects.end());
    return effects;
}


bool HMHeuristic::contradict_effect_of(
    const OperatorProxy &op, int var, int val) const {
    for (EffectProxy eff : op.get_effects()) {
        FactProxy fact = eff.get_fact();
        if (fact.get_variable().get_id() == var && fact.get_value() != val) {
            return true;
        }
    }
    return false;
}


void HMHeuristic::generate_all_tuples() {
    Tuple t;
    generate_all_tuples_aux(0, m, t);
}


void HMHeuristic::generate_all_tuples_aux(int var, int sz, const Tuple &base) {
    int num_variables = task_proxy.get_variables().size();
    for (int i = var; i < num_variables; ++i) {
        int domain_size = task_proxy.get_variables()[i].get_domain_size();
        for (int j = 0; j < domain_size; ++j) {
            Tuple tuple(base);
            tuple.emplace_back(i, j);
            hm_table[tuple] = 0;
            if (sz > 1) {
                generate_all_tuples_aux(i + 1, sz - 1, tuple);
            }
        }
    }
}


void HMHeuristic::generate_all_partial_tuples(
    const Tuple &base_tuple, vector<Tuple> &res) const {
    Tuple t;
    generate_all_partial_tuples_aux(base_tuple, t, 0, m, res);
}


void HMHeuristic::generate_all_partial_tuples_aux(
    const Tuple &base_tuple, const Tuple &t, int index, int sz, vector<Tuple> &res) const {
    if (sz == 1) {
        for (size_t i = index; i < base_tuple.size(); ++i) {
            Tuple tuple(t);
            tuple.push_back(base_tuple[i]);
            res.push_back(tuple);
        }
    } else {
        for (size_t i = index; i < base_tuple.size(); ++i) {
            Tuple tuple(t);
            tuple.push_back(base_tuple[i]);
            res.push_back(tuple);
            generate_all_partial_tuples_aux(base_tuple, tuple, i + 1, sz - 1, res);
        }
    }
}


void HMHeuristic::dump_table() const {
    for (auto &hm_ent : hm_table) {
        utils::g_log << "h(" << hm_ent.first << ") = " << hm_ent.second << endl;
    }
}


void HMHeuristic::setup_unsolvability_proof() {
    int varamount = task_proxy.get_variables().size();
    fact_to_variable.resize(varamount);
    strips_varamount = 0;
    for(int i = 0; i < varamount; ++i) {
        int domsize = task_proxy.get_variables()[i].get_domain_size();
        fact_to_variable[i].resize(domsize);
        for(int j = 0; j < domsize; ++j) {
            // we want the variables to start with 1 since that is how the DIMACS format works
            fact_to_variable[i][j] = ++strips_varamount;
        }
    }

    mutexamount = 0;
    std::stringstream ss;
    for(int i = 0; i < varamount; ++i) {
        int domsize = task_proxy.get_variables()[i].get_domain_size();
        for(int j = 0; j < domsize-1; ++j) {
            for(int k = j+1; k < domsize; ++k) {
                ss << "-" << fact_to_variable[i][j] << " -" << fact_to_variable[i][k] << " 0 ";
                mutexamount++;
            }
        }
    }
    mutexes = ss.str();
    unsolvability_setup = true;
}

void HMHeuristic::store_deadend_info(EvaluationContext &eval_context) {
    if(!unsolvability_setup) {
        setup_unsolvability_proof();
    }

    std::forward_list<const Tuple *> tuples;
    for(auto &elem : hm_table) {
        if (elem.second == numeric_limits<int>::max()) {
            tuples.push_front(&(elem.first));
        }
    }
    unreachable_tuples.insert({eval_context.get_state().get_id().get_value(), std::move(tuples)});
}

std::pair<int,int> HMHeuristic::get_set_and_deadknowledge_id(
        EvaluationContext &eval_context, UnsolvabilityManager &unsolvmanager) {

    int clauseamount = mutexamount;
    std::stringstream tuples;
    for(const Tuple * tuple : unreachable_tuples[eval_context.get_state().get_id().get_value()]) {
        for(size_t i = 0; i < tuple->size(); ++i) {
            tuples << "-" << fact_to_variable[tuple->at(i).var][tuple->at(i).value] << " ";
        }
        tuples << "0 ";
        clauseamount++;
    }

    int setid = unsolvmanager.get_new_setid();
    std::ofstream &certstream = unsolvmanager.get_stream();
    certstream << "e " << setid << " h p cnf " << strips_varamount << " " << clauseamount << " ";
    certstream << mutexes << tuples.str() << ";\n";

    int progid = unsolvmanager.get_new_setid();
    certstream << "e " << progid << " p " << setid << " 0" << "\n";
    int union_set_empty = unsolvmanager.get_new_setid();;
    certstream << "e " << union_set_empty << " u "
               << setid << " " << unsolvmanager.get_emptysetid() << "\n";

    int k_prog = unsolvmanager.get_new_knowledgeid();
    certstream << "k " << k_prog << " s " << progid << " " << union_set_empty << " b2\n";

    int set_and_goal = unsolvmanager.get_new_setid();
    certstream << "e " << set_and_goal << " i "
               << setid << " " << unsolvmanager.get_goalsetid() << "\n";
    int k_set_and_goal_empty = unsolvmanager.get_new_knowledgeid();
    certstream << "k " << k_set_and_goal_empty << " s "
               << set_and_goal << " " << unsolvmanager.get_emptysetid() << " b1\n";
    int k_set_and_goal_dead = unsolvmanager.get_new_knowledgeid();
    certstream << "k " << k_set_and_goal_dead << " d " << set_and_goal
               << " sd " << k_set_and_goal_empty << " " << unsolvmanager.get_k_empty_dead() << "\n";

    int k_set_dead = unsolvmanager.get_new_knowledgeid();
    certstream << "k " << k_set_dead << " d " << setid << " pg " << k_prog << " "
               << unsolvmanager.get_k_empty_dead() << " " << k_set_and_goal_dead << "\n";

    return std::make_pair(setid, k_set_dead);
}


static shared_ptr<Heuristic> _parse(OptionParser &parser) {
    parser.document_synopsis("h^m heuristic", "");
    parser.document_language_support("action costs", "supported");
    parser.document_language_support("conditional effects", "ignored");
    parser.document_language_support("axioms", "ignored");
    parser.document_property("admissible",
                             "yes for tasks without conditional "
                             "effects or axioms");
    parser.document_property("consistent",
                             "yes for tasks without conditional "
                             "effects or axioms");
    parser.document_property("safe",
                             "yes for tasks without conditional "
                             "effects or axioms");
    parser.document_property("preferred operators", "no");

    parser.add_option<int>("m", "subset size", "2", Bounds("1", "infinity"));
    Heuristic::add_options_to_parser(parser);
    Options opts = parser.parse();
    if (parser.dry_run())
        return nullptr;
    else
        return make_shared<HMHeuristic>(opts);
}

static Plugin<Evaluator> _plugin("hm", _parse);
}
