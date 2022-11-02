#include "certificatemanager.h"

#include "../state_registry.h"

#include "../utils/system.h"
#include "../task_proxy.h"

#include <limits>

CertificateManager::CertificateManager(
        std::string directory, std::shared_ptr<AbstractTask> task)
    : task(task), task_proxy(*task), actionset_count(0), stateset_count(0),
      knowledge_count(0), emptyset(stateset_count++), goalset(stateset_count++),
      initset(stateset_count++), allactions(actionset_count++), directory(directory) {
    certstream.open(directory + "proof.txt");
    certstream << "e " << emptyset.id << " c e\n";
    certstream << "e " << goalset.id << " c g\n";
    certstream << "e " << initset.id << " c i\n";
    certstream << "a " << allactions.id << " a\n";

    hex = {'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e' , 'f'};
}

size_t CertificateManager::apply_bound_rule(size_t setid, size_t bound, std::string rulename,
                                          std::vector<size_t> justification) {
    int new_kid = get_new_knowledgeid();
    certstream << "k " << new_kid << " b " << setid << " " << bound << " " << rulename;
    for (int jid : justification) {
        certstream << " " << jid;
    }
    certstream << "\n";
    return new_kid;
}

size_t CertificateManager::apply_subset_rule(size_t left_setid, size_t right_setid,
                                          std::string rulename,
                                          std::vector<size_t> justification) {
    int new_kid = get_new_knowledgeid();
    certstream << "k " << new_kid << " s " << left_setid << " " << right_setid << rulename;
    for (int jid : justification)  {
        certstream << " "  << jid;
    }
    certstream << "\n";
    return new_kid;
}

size_t CertificateManager::get_new_actionsetid() {
    std::cout << "actionset count: " << actionset_count << std::endl;
    return actionset_count++;
}
size_t CertificateManager::get_new_statesetid() {
    return stateset_count++;
}
size_t CertificateManager::get_new_knowledgeid() {
    return knowledge_count++;
}

const SetExpression &CertificateManager::get_emptyset() {
    return emptyset;
}
const SetExpression &CertificateManager::get_goalset() {
    return goalset;
}
const SetExpression &CertificateManager::get_initset() {
    return initset;
}
const SetExpression &CertificateManager::get_allactions() {
    return allactions;
}

void CertificateManager::dump_state(const State &state) {
    int c = 0;
    int count = 3;
    for(FactProxy fact : state) {
        for(int j = 0; j < fact.get_variable().get_domain_size(); ++j) {
            if(fact.get_value() == j) {
                c += (1 << count);
            }
            count--;
            if(count==-1) {
                certstream << hex[c];
                c = 0;
                count = 3;
            }
        }
    }
    if(count != 3) {
        certstream << hex[c];
    }
}

void CertificateManager::dump_BDDs() {
    for (auto entry : bdds) {
        const CuddManager *manager = entry.first;
        manager->dumpBDDs(entry.second, directory);
    }
}

SetExpression CertificateManager::define_action(std::vector<int> action_indices) {
    size_t new_sid = get_new_actionsetid();
    certstream << "a " << new_sid << " b " << action_indices.size();
    for (int index : action_indices) {
        certstream << " " << index;
    }
    certstream << "\n";
    return SetExpression(new_sid);
}

SetExpression CertificateManager::define_action_set_union(const SetExpression &left_set,
                                                          const SetExpression &right_set) {
    size_t new_sid = get_new_actionsetid();
    certstream << "a " << new_sid << " u " << left_set.id << " " << right_set.id << "\n";
    return SetExpression(new_sid);
}


// TODO: enforce move
SetExpression CertificateManager::define_bdd(CuddBDD bdd) {
    CuddManager *manager = bdd.get_manager();
    int pos = bdds[manager].size();
    bdds[manager].push_back(bdd);
    size_t new_sid = get_new_statesetid();
    certstream << "e " << new_sid << " b " << directory + manager->get_filename() << " " << pos << " ;\n";
    return SetExpression(new_sid);
}

SetExpression CertificateManager::define_horn_formula(int varamount, std::vector<std::vector<int> > &clauses) {
    size_t new_sid = get_new_statesetid();
    certstream << "e " << new_sid << " h p cnf " << varamount << " " << clauses.size() << " ";
    for (std::vector<int> clause : clauses) {
        for (int prop : clause) {
            certstream << prop << " ";
        }
        certstream << "0 ";
    }
    certstream << ";\n";
    return SetExpression(new_sid);
}

SetExpression CertificateManager::define_explicit_set(int fact_amount, StateRegistry &state_registry, std::vector<StateID> state_ids) {
    size_t new_sid = get_new_statesetid();
    certstream << "e " << new_sid << " e ";
    certstream << fact_amount << " ";
    for (int i = 0; i < fact_amount; ++i) {
        certstream << i << " ";
    }
    certstream << ": ";
    for(const StateID &id : state_ids) {
        dump_state(state_registry.lookup_state(id));
        certstream << " ";
    }
    certstream << ";\n";
    return SetExpression(new_sid);
}

SetExpression CertificateManager::define_set_negation(const SetExpression &set) {
    size_t new_sid = get_new_statesetid();
    certstream << "e " << new_sid << " n " << set.id << "\n";
    return SetExpression(new_sid);
}
SetExpression CertificateManager::define_set_union(const SetExpression &left_set, const SetExpression &right_set) {
    size_t new_sid = get_new_statesetid();
    certstream << "e " << new_sid << " u " << left_set.id << " " << right_set.id << "\n";
    return SetExpression(new_sid);
}
SetExpression CertificateManager::define_set_intersection(const SetExpression &left_set, const SetExpression &right_set) {
    size_t new_sid = get_new_statesetid();
    certstream << "e " << new_sid << " i " << left_set.id << " " << right_set.id << " \n";
    return SetExpression(new_sid);
}
SetExpression CertificateManager::define_set_progression(const SetExpression &set, const SetExpression &actionset) {
    size_t new_sid = get_new_statesetid();
    certstream << "e " << new_sid << " p " << set.id << " " << actionset.id << "\n";
    return SetExpression(new_sid);
}
SetExpression CertificateManager::define_set_regression(const SetExpression &set, const SetExpression &actionset) {
    size_t new_sid = get_new_statesetid();
    certstream << "e " << new_sid << " r " << set.id << " " << actionset.id << "\n";
    return SetExpression(new_sid);
}

Judgment CertificateManager::make_statement(const SetExpression &left_set, const SetExpression &right_set, std::string type) {
    size_t new_kid = get_new_knowledgeid();
    certstream << "k " << new_kid << " s " << left_set.id << " " << right_set.id << " " << type << "\n";
    return Judgment(new_kid);
}


Judgment CertificateManager::apply_rule_ec() {
    return Judgment(apply_bound_rule(emptyset.id, std::numeric_limits<size_t>::max(),
                                     "ec", std::vector<size_t>()));
}
Judgment CertificateManager::apply_rule_tc(const SetExpression set) {
    return Judgment(apply_bound_rule(set.id, 0, "tc", std::vector<size_t>()));
}
Judgment CertificateManager::apply_rule_sc(const SetExpression set, unsigned bound,
                                           const Judgment &sp_bound,
                                           const Judgment &s_subset_sp) {
    return Judgment(apply_bound_rule(set.id, bound, "sc", {sp_bound.id, s_subset_sp.id}));
}
Judgment CertificateManager::apply_rule_uc(const SetExpression set, unsigned bound,
                                           const Judgment &s_bound,
                                           const Judgment &sp_bound) {
    return Judgment(apply_bound_rule(set.id, bound, "uc", {s_bound.id, sp_bound.id}));
}
Judgment CertificateManager::apply_rule_pc(const SetExpression set, unsigned bound,
                                           const Judgment &actions_contained, const Judgment &nogoal,
                                           std::vector<std::pair<Judgment,Judgment>> successor_bounds) {
    std::vector<size_t> justification = {actions_contained.id, nogoal.id};
    justification.reserve(2*successor_bounds.size()+2);
    for (auto succ_bound : successor_bounds) {
        justification.push_back(succ_bound.first.id);
        justification.push_back(succ_bound.second.id);
    }
    return Judgment(apply_bound_rule(set.id, bound, "pc", justification));
}

Judgment CertificateManager::apply_rule_ur(const SetExpression &left_set, const SetExpression &right_set) {
    return Judgment(apply_subset_rule(left_set.id, right_set.id, "ur", {}));
}
Judgment CertificateManager::apply_rule_ul(const SetExpression &left_set, const SetExpression &right_set) {
    return Judgment(apply_subset_rule(left_set.id, right_set.id, "ul", {}));
}
Judgment CertificateManager::apply_rule_ir(const SetExpression &left_set, const SetExpression &right_set) {
    return Judgment(apply_subset_rule(left_set.id, right_set.id, "ir", {}));
}
Judgment CertificateManager::apply_rule_il(const SetExpression &left_set, const SetExpression &right_set) {
    return Judgment(apply_subset_rule(left_set.id, right_set.id, "il", {}));
}
Judgment CertificateManager::apply_rule_di(const SetExpression &left_set, const SetExpression &right_set) {
    return Judgment(apply_subset_rule(left_set.id, right_set.id, "di", {}));
}
Judgment CertificateManager::apply_rule_su(const SetExpression &left_set, const SetExpression &right_set,
                                                   const Judgment &e_subset_epp,
                                                   const Judgment &ep_subset_epp) {
    return Judgment(apply_subset_rule(left_set.id, right_set.id, "su", {e_subset_epp.id, ep_subset_epp.id}));
}
Judgment CertificateManager::apply_rule_si(const SetExpression &left_set, const SetExpression &right_set,
                                                   const Judgment &e_subset_ep,
                                                   const Judgment &e_subset_epp) {
    return Judgment(apply_subset_rule(left_set.id, right_set.id, "si", {e_subset_ep.id, e_subset_epp.id}));
}
Judgment CertificateManager::apply_rule_st(const SetExpression &left_set, const SetExpression &right_set,
                                                   const Judgment &e_subset_ep,
                                                   const Judgment &ep_subset_epp) {
    return Judgment(apply_subset_rule(left_set.id, right_set.id, "st", {e_subset_ep.id, ep_subset_epp.id}));
}

Judgment CertificateManager::apply_rule_at(const SetExpression &left_set, const SetExpression &right_set,
                                                   const Judgment &prog_subset,
                                                   const Judgment &a_subset) {
    return Judgment(apply_subset_rule(left_set.id, right_set.id, "at", {prog_subset.id, a_subset.id}));
}
Judgment CertificateManager::apply_rule_au(const SetExpression &left_set, const SetExpression &right_set,
                                                   const Judgment &a_prog_subset,
                                                   const Judgment &ap_prog_subset) {
    return Judgment(apply_subset_rule(left_set.id, right_set.id, "au", {a_prog_subset.id, ap_prog_subset.id}));
}
Judgment CertificateManager::apply_rule_pt(const SetExpression &left_set, const SetExpression &right_set,
                                                   const Judgment &prog_subset,
                                                   const Judgment &sp_subset_s) {
    return Judgment(apply_subset_rule(left_set.id, right_set.id, "pt", {prog_subset.id, sp_subset_s.id}));
}
Judgment CertificateManager::apply_rule_pu(const SetExpression &left_set, const SetExpression &right_set,
                                                   const Judgment &s_prog_subset,
                                                   const Judgment &sp_prog_subset) {
    return Judgment(apply_subset_rule(left_set.id, right_set.id, "pu", {s_prog_subset.id, sp_prog_subset.id}));
}
Judgment CertificateManager::apply_rule_pr(const SetExpression &left_set, const SetExpression &right_set,
                                                   const Judgment &progression) {
    return Judgment(apply_subset_rule(left_set.id, right_set.id, "pr", {progression.id}));
}
Judgment CertificateManager::apply_rule_rp(const SetExpression &left_set, const SetExpression &right_set,
                                                   const Judgment &regression) {
    return Judgment(apply_subset_rule(left_set.id, right_set.id, "rp", {regression.id}));
}
