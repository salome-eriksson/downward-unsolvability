#include "unsolvabilitymanager.h"

#include "../utils/system.h"
#include "../task_proxy.h"


UnsolvabilityManager::UnsolvabilityManager(
        std::string directory, std::shared_ptr<AbstractTask> task)
    : task(task), task_proxy(*task), setcount(0), knowledgecount(0), directory(directory) {
    certstream.open(directory + "proof.txt");

    emptysetid = setcount++;
    certstream << "e " << emptysetid << " c e\n";
    goalsetid = setcount++;
    certstream << "e " << goalsetid << " c g\n";
    initsetid = setcount++;
    certstream << "e " << initsetid << " c i\n";
    k_empty_dead = knowledgecount++;
    certstream << "k " << k_empty_dead << " d " << emptysetid << " ed\n";
    certstream << "a 0 a\n";

    hex = {'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e' , 'f'};
}

int UnsolvabilityManager::apply_dead_rule(int setid, std::string rulename,
                                          std::vector<int> justification) {
    int kid = get_new_knowledgeid();
    certstream << "k " << kid << " d " << setid << " " << rulename;
    for (int jid : justification) {
        certstream << " " << jid;
    }
    certstream << "\n";
    return kid;
}

int UnsolvabilityManager::apply_subset_rule(int left_setid, int right_setid, std::string rulename,
                                            std::vector<int> justification) {
    int kid = get_new_knowledgeid();
    certstream << "k " << kid << " s " << left_setid << " " << right_setid << rulename;
    for (int jid : justification)  {
        certstream << " "  << jid;
    }
    certstream << "\n";
    return kid;
}

int UnsolvabilityManager::get_new_setid() {
    return setcount++;
}
int UnsolvabilityManager::get_new_knowledgeid() {
    return knowledgecount++;
}

int UnsolvabilityManager::get_emptysetid() {
    return emptysetid;
}
int UnsolvabilityManager::get_goalsetid() {
    return goalsetid;
}
int UnsolvabilityManager::get_initsetid() {
    return initsetid;
}
int UnsolvabilityManager::get_k_empty_dead() {
    return k_empty_dead;
}

std::ofstream &UnsolvabilityManager::get_stream() {
    return certstream;
}

std::string &UnsolvabilityManager::get_directory() {
    return directory;
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

Judgment UnsolvabilityManager::make_statement(int left_setid, int right_setid, std::string type) {
    int kid = get_new_knowledgeid();
    certstream << "k " << kid << " s " << left_setid << " " << right_setid << " " << type << "\n";
    return Judgment(kid);
}

Judgment UnsolvabilityManager::apply_rule_ed() {
    return Judgment(k_empty_dead);
}
Judgment UnsolvabilityManager::apply_rule_ud(int setid, Judgment &s_dead,
                                                   Judgment &sp_dead) {
    return Judgment(apply_dead_rule(setid, "ud", {s_dead.id, sp_dead.id}));
}
Judgment UnsolvabilityManager::apply_rule_sd(int setid, Judgment &s_subset_sp,
                                                   Judgment &sp_dead) {
    return Judgment(apply_dead_rule(setid, "sd", {s_subset_sp.id, sp_dead.id}));
}
Judgment UnsolvabilityManager::apply_rule_pg(int setid, Judgment &progression_subset,
                                                   Judgment &sp_dead,
                                                   Judgment &goal_intersection_dead) {
    return Judgment(apply_dead_rule(setid, "pg", {progression_subset.id, sp_dead.id, goal_intersection_dead.id}));
}
Judgment UnsolvabilityManager::apply_rule_pi(int setid, Judgment &progression_subset,
                                                   Judgment &sp_dead,
                                                   Judgment &init_subset) {
    return Judgment(apply_dead_rule(setid, "pi", {progression_subset.id, sp_dead.id, init_subset.id}));
}
Judgment UnsolvabilityManager::apply_rule_rg(int setid, Judgment &regression_subset,
                                                   Judgment &sp_dead,
                                                   Judgment &goal_intersection_dead) {
    return Judgment(apply_dead_rule(setid, "rg", {regression_subset.id, sp_dead.id, goal_intersection_dead.id}));
}
Judgment UnsolvabilityManager::apply_rule_ri(int setid, Judgment &regression_subset,
                                                   Judgment &sp_dead,
                                                   Judgment &init_subset) {
    return Judgment(apply_dead_rule(setid, "ri", {regression_subset.id, sp_dead.id, init_subset.id}));
}

Judgment UnsolvabilityManager::apply_rule_ci(Judgment &init_dead) {
    int kid = get_new_knowledgeid();
    certstream << "k " << kid << " u ci " << init_dead.id << "\n";
    return Judgment(kid);
}
Judgment UnsolvabilityManager::apply_rule_cg(Judgment &goal_dead) {
    int kid = get_new_knowledgeid();
    certstream << "k " << kid << "u cg " << goal_dead.id << "\n";
    return Judgment(kid);
}

Judgment UnsolvabilityManager::apply_rule_ur(int left_setid, int right_setid) {
    return Judgment(apply_subset_rule(left_setid, right_setid, "ur", {}));
}
Judgment UnsolvabilityManager::apply_rule_ul(int left_setid, int right_setid) {
    return Judgment(apply_subset_rule(left_setid, right_setid, "ul", {}));
}
Judgment UnsolvabilityManager::apply_rule_ir(int left_setid, int right_setid) {
    return Judgment(apply_subset_rule(left_setid, right_setid, "ir", {}));
}
Judgment UnsolvabilityManager::apply_rule_il(int left_setid, int right_setid) {
    return Judgment(apply_subset_rule(left_setid, right_setid, "il", {}));
}
Judgment UnsolvabilityManager::apply_rule_di(int left_setid, int right_setid) {
    return Judgment(apply_subset_rule(left_setid, right_setid, "di", {}));
}
Judgment UnsolvabilityManager::apply_rule_su(int left_setid, int right_setid,
                                                   Judgment &e_subset_epp,
                                                   Judgment &ep_subset_epp) {
    return Judgment(apply_subset_rule(left_setid, right_setid, "su", {e_subset_epp.id, ep_subset_epp.id}));
}
Judgment UnsolvabilityManager::apply_rule_si(int left_setid, int right_setid,
                                                   Judgment &e_subset_ep,
                                                   Judgment &e_subset_epp) {
    return Judgment(apply_subset_rule(left_setid, right_setid, "si", {e_subset_ep.id, e_subset_epp.id}));
}
Judgment UnsolvabilityManager::apply_rule_st(int left_setid, int right_setid,
                                                   Judgment &e_subset_ep,
                                                   Judgment &ep_subset_epp) {
    return Judgment(apply_subset_rule(left_setid, right_setid, "st", {e_subset_ep.id, ep_subset_epp.id}));
}

Judgment UnsolvabilityManager::apply_rule_at(int left_setid, int right_setid,
                                                   Judgment &prog_subset,
                                                   Judgment &a_subset) {
    return Judgment(apply_subset_rule(left_setid, right_setid, "at", {prog_subset.id, a_subset.id}));
}
Judgment UnsolvabilityManager::apply_rule_au(int left_setid, int right_setid,
                                                   Judgment &a_prog_subset,
                                                   Judgment &ap_prog_subset) {
    return Judgment(apply_subset_rule(left_setid, right_setid, "au", {a_prog_subset.id, ap_prog_subset.id}));
}
Judgment UnsolvabilityManager::apply_rule_pt(int left_setid, int right_setid,
                                                   Judgment &prog_subset,
                                                   Judgment &sp_subset_s) {
    return Judgment(apply_subset_rule(left_setid, right_setid, "pt", {prog_subset.id, sp_subset_s.id}));
}
Judgment UnsolvabilityManager::apply_rule_pu(int left_setid, int right_setid,
                                                   Judgment &s_prog_subset,
                                                   Judgment &sp_prog_subset) {
    return Judgment(apply_subset_rule(left_setid, right_setid, "pu", {s_prog_subset.id, sp_prog_subset.id}));
}
Judgment UnsolvabilityManager::apply_rule_pr(int left_setid, int right_setid,
                                                   Judgment &progression) {
    return Judgment(apply_subset_rule(left_setid, right_setid, "pr", {progression.id}));
}
Judgment UnsolvabilityManager::apply_rule_rp(int left_setid, int right_setid,
                                                   Judgment &regression) {
    return Judgment(apply_subset_rule(left_setid, right_setid, "rp", {regression.id}));
}
