#include "unsolvabilitymanager.h"

#include "../state_registry.h"

#include "../utils/system.h"
#include "../task_proxy.h"


UnsolvabilityManager::UnsolvabilityManager(
        std::string directory, std::shared_ptr<AbstractTask> task)
    : task(task), task_proxy(*task), setcount(0), knowledgecount(0), directory(directory) {
    certstream.open(directory + "proof.txt");
    certstream << "a 0 a\n";

    hex = {'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e' , 'f'};
}

int UnsolvabilityManager::apply_dead_rule(int setid, std::string rulename,
                                          std::vector<int> justification) {
    int new_kid = get_new_knowledgeid();
    certstream << "k " << new_kid << " d " << setid << " " << rulename;
    for (int jid : justification) {
        certstream << " " << jid;
    }
    certstream << "\n";
    return new_kid;
}

int UnsolvabilityManager::apply_subset_rule(int left_setid, int right_setid, std::string rulename,
                                            std::vector<int> justification) {
    int new_kid = get_new_knowledgeid();
    certstream << "k " << new_kid << " s " << left_setid << " " << right_setid << rulename;
    for (int jid : justification)  {
        certstream << " "  << jid;
    }
    certstream << "\n";
    return new_kid;
}

int UnsolvabilityManager::get_new_setid() {
    return setcount++;
}
int UnsolvabilityManager::get_new_knowledgeid() {
    return knowledgecount++;
}

SetExpression UnsolvabilityManager::get_emptyset() {
    if (emptyset.id == -1) {
        int new_sid = get_new_setid();
        certstream << "e " << new_sid << " c e\n";
        emptyset = SetExpression(new_sid);
    }
    return emptyset;
}
SetExpression UnsolvabilityManager::get_goalset() {
    if (goalset.id == -1) {
        int new_sid = get_new_setid();
        certstream << "e " << new_sid << " c g\n";
        goalset = SetExpression(new_sid);
    }
    return goalset;
}
SetExpression UnsolvabilityManager::get_initset() {
    if (initset.id == -1) {
        int new_sid = get_new_setid();
        certstream << "e " << new_sid << " c i\n";
        initset = SetExpression(new_sid);
    }
    return initset;
}

void UnsolvabilityManager::dump_state(const State &state) {
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

void UnsolvabilityManager::dump_BDDs() {
    for (auto entry : bdds) {
        const CuddManager *manager = entry.first;
        manager->dumpBDDs(entry.second, directory);
    }
}


// TODO: enforce move
SetExpression UnsolvabilityManager::define_bdd(CuddBDD bdd) {
    CuddManager *manager = bdd.get_manager();
    int pos = bdds[manager].size();
    bdds[manager].push_back(bdd);
    int new_sid = get_new_setid();
    certstream << "e " << new_sid << " b " << directory + manager->get_filename() << " " << pos << " ;\n";
    return SetExpression(new_sid);
}

SetExpression UnsolvabilityManager::define_horn_formula(int varamount, std::vector<std::vector<int> > &clauses) {
    int new_sid = get_new_setid();
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

SetExpression UnsolvabilityManager::define_explicit_set(int fact_amount, StateRegistry &state_registry, std::vector<StateID> state_ids) {
    int new_sid = get_new_setid();
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

SetExpression UnsolvabilityManager::define_set_negation(SetExpression &set) {
    int new_sid = get_new_setid();
    certstream << "e " << new_sid << " n " << set.id << "\n";
    return SetExpression(new_sid);
}
SetExpression UnsolvabilityManager::define_set_union(SetExpression &left_set, SetExpression &right_set) {
    int new_sid = get_new_setid();
    certstream << "e " << new_sid << " u " << left_set.id << " " << right_set.id << "\n";
    return SetExpression(new_sid);
}
SetExpression UnsolvabilityManager::define_set_intersection(SetExpression &left_set, SetExpression &right_set) {
    int new_sid = get_new_setid();
    certstream << "e " << new_sid << " i " << left_set.id << " " << right_set.id << " \n";
    return SetExpression(new_sid);
}
SetExpression UnsolvabilityManager::define_set_progression(SetExpression &set, int actionset_id) {
    int new_sid = get_new_setid();
    certstream << "e " << new_sid << " p " << set.id << " " << actionset_id << "\n";
    return SetExpression(new_sid);
}
SetExpression UnsolvabilityManager::define_set_regression(SetExpression &set, int actionset_id) {
    int new_sid = get_new_setid();
    certstream << "e " << new_sid << " r " << set.id << " " << actionset_id << "\n";
    return SetExpression(new_sid);
}

Judgment UnsolvabilityManager::make_statement(SetExpression left_set, SetExpression right_set, std::string type) {
    int new_kid = get_new_knowledgeid();
    certstream << "k " << new_kid << " s " << left_set.id << " " << right_set.id << " " << type << "\n";
    return Judgment(new_kid);
}

Judgment UnsolvabilityManager::apply_rule_ed() {
    if (empty_dead.id == -1) {
        empty_dead = Judgment(apply_dead_rule(get_emptyset().id, "ed", {}));
    }
    return empty_dead;
}
Judgment UnsolvabilityManager::apply_rule_ud(SetExpression set, Judgment &s_dead,
                                                   Judgment &sp_dead) {
    return Judgment(apply_dead_rule(set.id, "ud", {s_dead.id, sp_dead.id}));
}
Judgment UnsolvabilityManager::apply_rule_sd(SetExpression set, Judgment &sp_dead,
                                                   Judgment &s_subset_sp) {
    return Judgment(apply_dead_rule(set.id, "sd", {sp_dead.id, s_subset_sp.id}));
}
Judgment UnsolvabilityManager::apply_rule_pg(SetExpression set, Judgment &progression_subset,
                                                   Judgment &sp_dead,
                                                   Judgment &goal_intersection_dead) {
    return Judgment(apply_dead_rule(set.id, "pg", {progression_subset.id, sp_dead.id, goal_intersection_dead.id}));
}
Judgment UnsolvabilityManager::apply_rule_pi(SetExpression set, Judgment &progression_subset,
                                                   Judgment &sp_dead,
                                                   Judgment &init_subset) {
    return Judgment(apply_dead_rule(set.id, "pi", {progression_subset.id, sp_dead.id, init_subset.id}));
}
Judgment UnsolvabilityManager::apply_rule_rg(SetExpression set, Judgment &regression_subset,
                                                   Judgment &sp_dead,
                                                   Judgment &goal_intersection_dead) {
    return Judgment(apply_dead_rule(set.id, "rg", {regression_subset.id, sp_dead.id, goal_intersection_dead.id}));
}
Judgment UnsolvabilityManager::apply_rule_ri(SetExpression set, Judgment &regression_subset,
                                                   Judgment &sp_dead,
                                                   Judgment &init_subset) {
    return Judgment(apply_dead_rule(set.id, "ri", {regression_subset.id, sp_dead.id, init_subset.id}));
}

Judgment UnsolvabilityManager::apply_rule_ci(Judgment &init_dead) {
    int new_kid = get_new_knowledgeid();
    certstream << "k " << new_kid << " u ci " << init_dead.id << "\n";
    return Judgment(new_kid);
}
Judgment UnsolvabilityManager::apply_rule_cg(Judgment &goal_dead) {
    int new_kid = get_new_knowledgeid();
    certstream << "k " << new_kid << "u cg " << goal_dead.id << "\n";
    return Judgment(new_kid);
}

Judgment UnsolvabilityManager::apply_rule_ur(SetExpression left_set, SetExpression right_set) {
    return Judgment(apply_subset_rule(left_set.id, right_set.id, "ur", {}));
}
Judgment UnsolvabilityManager::apply_rule_ul(SetExpression left_set, SetExpression right_set) {
    return Judgment(apply_subset_rule(left_set.id, right_set.id, "ul", {}));
}
Judgment UnsolvabilityManager::apply_rule_ir(SetExpression left_set, SetExpression right_set) {
    return Judgment(apply_subset_rule(left_set.id, right_set.id, "ir", {}));
}
Judgment UnsolvabilityManager::apply_rule_il(SetExpression left_set, SetExpression right_set) {
    return Judgment(apply_subset_rule(left_set.id, right_set.id, "il", {}));
}
Judgment UnsolvabilityManager::apply_rule_di(SetExpression left_set, SetExpression right_set) {
    return Judgment(apply_subset_rule(left_set.id, right_set.id, "di", {}));
}
Judgment UnsolvabilityManager::apply_rule_su(SetExpression left_set, SetExpression right_set,
                                                   Judgment &e_subset_epp,
                                                   Judgment &ep_subset_epp) {
    return Judgment(apply_subset_rule(left_set.id, right_set.id, "su", {e_subset_epp.id, ep_subset_epp.id}));
}
Judgment UnsolvabilityManager::apply_rule_si(SetExpression left_set, SetExpression right_set,
                                                   Judgment &e_subset_ep,
                                                   Judgment &e_subset_epp) {
    return Judgment(apply_subset_rule(left_set.id, right_set.id, "si", {e_subset_ep.id, e_subset_epp.id}));
}
Judgment UnsolvabilityManager::apply_rule_st(SetExpression left_set, SetExpression right_set,
                                                   Judgment &e_subset_ep,
                                                   Judgment &ep_subset_epp) {
    return Judgment(apply_subset_rule(left_set.id, right_set.id, "st", {e_subset_ep.id, ep_subset_epp.id}));
}

Judgment UnsolvabilityManager::apply_rule_at(SetExpression left_set, SetExpression right_set,
                                                   Judgment &prog_subset,
                                                   Judgment &a_subset) {
    return Judgment(apply_subset_rule(left_set.id, right_set.id, "at", {prog_subset.id, a_subset.id}));
}
Judgment UnsolvabilityManager::apply_rule_au(SetExpression left_set, SetExpression right_set,
                                                   Judgment &a_prog_subset,
                                                   Judgment &ap_prog_subset) {
    return Judgment(apply_subset_rule(left_set.id, right_set.id, "au", {a_prog_subset.id, ap_prog_subset.id}));
}
Judgment UnsolvabilityManager::apply_rule_pt(SetExpression left_set, SetExpression right_set,
                                                   Judgment &prog_subset,
                                                   Judgment &sp_subset_s) {
    return Judgment(apply_subset_rule(left_set.id, right_set.id, "pt", {prog_subset.id, sp_subset_s.id}));
}
Judgment UnsolvabilityManager::apply_rule_pu(SetExpression left_set, SetExpression right_set,
                                                   Judgment &s_prog_subset,
                                                   Judgment &sp_prog_subset) {
    return Judgment(apply_subset_rule(left_set.id, right_set.id, "pu", {s_prog_subset.id, sp_prog_subset.id}));
}
Judgment UnsolvabilityManager::apply_rule_pr(SetExpression left_set, SetExpression right_set,
                                                   Judgment &progression) {
    return Judgment(apply_subset_rule(left_set.id, right_set.id, "pr", {progression.id}));
}
Judgment UnsolvabilityManager::apply_rule_rp(SetExpression left_set, SetExpression right_set,
                                                   Judgment &regression) {
    return Judgment(apply_subset_rule(left_set.id, right_set.id, "rp", {regression.id}));
}
