#ifndef CERTIFICATEMANAGER_H
#define CERTIFICATEMANAGER_H

#include "cudd_interface.h"

#include "../abstract_task.h"
#include "../task_proxy.h"

#include <fstream>

class Judgment {
    friend class CertificateManager;
    int id;

    Judgment(int id)
        : id(id) {
    }

public:
    /*
     * TODO: ideally, I don't want to implement default construction
     * However, M&S needs this since it has a Judgment member
     */
    Judgment()
        : id(-1) {
    }
    ~Judgment() {
    }
};

class SetExpression {
    friend class CertificateManager;
    int id;

    SetExpression(int id)
        : id(id) {
    }
public:
    /*
     * TODO: ideally, I don't want to implement default construction
     * However, eager_serach needs this since it has a SetExpression variable
     */
    SetExpression()
        : id(-1) {
    }
    ~SetExpression() {
    }
};

class CertificateManager {
private:
    std::shared_ptr<AbstractTask> task;
    TaskProxy task_proxy;

    int setcount;
    int knowledgecount;

    SetExpression emptyset;
    SetExpression goalset;
    SetExpression initset;
    Judgment empty_dead;

    std::ofstream certstream;

    std::string directory;
    std::vector<char> hex;

    std::unordered_map<CuddManager *, std::vector<CuddBDD>> bdds;

    int get_new_setid();
    int get_new_knowledgeid();

    int apply_dead_rule(int setid, std::string rulename, std::vector<int> justification);
    int apply_subset_rule(int left_setid, int right_setid, std::string rulename, std::vector<int> justification);

public:
    CertificateManager(std::string directory, std::shared_ptr<AbstractTask> task);


    SetExpression get_emptyset();
    SetExpression get_goalset();
    SetExpression get_initset();

    void dump_state(const State &state);

    void dump_BDDs();

    // TODO: should we return only const Judgments and SetExpressions?

    // TODO: make BDD const (currently a problem when getting the manager)
    SetExpression define_bdd(CuddBDD bdd);
    SetExpression define_horn_formula(int varamount, std::vector<std::vector<int>> &clauses);
    SetExpression define_explicit_set(int fact_amount, StateRegistry &state_registry, std::vector<StateID> state_ids);

    SetExpression define_set_negation(SetExpression &set);
    SetExpression define_set_union(SetExpression &left_set, SetExpression &right_set);
    SetExpression define_set_intersection(SetExpression &left_set, SetExpression &right_set);
    // TODO: either define actionsets or remove parameter and always write 0
    SetExpression define_set_progression(SetExpression &set, int actionset_id);
    SetExpression define_set_regression(SetExpression &set, int actionset_id);

    // TODO: have sets instead of setids
    Judgment make_statement(SetExpression left_set, SetExpression right_set, std::string type);

    Judgment apply_rule_ed(); // TODO: only apply this once and return the proper Judgment on subsequent calls
    Judgment apply_rule_ud(SetExpression set, Judgment &s_dead, Judgment &sp_dead);
    // TODO: in the thesis, the judgments for SD are the other way around
    Judgment apply_rule_sd(SetExpression set, Judgment &s_subset_sp, Judgment &sp_dead);
    Judgment apply_rule_pg(SetExpression set, Judgment &progression_subset,
                           Judgment &sp_dead,
                           Judgment &goal_intersection_dead);
    Judgment apply_rule_pi(SetExpression set, Judgment &progression_subset,
                           Judgment &sp_dead,
                           Judgment &init_subset);
    Judgment apply_rule_rg(SetExpression set, Judgment &regression_subset,
                           Judgment &sp_dead,
                           Judgment &goal_intersection_dead);
    Judgment apply_rule_ri(SetExpression set, Judgment &regression_subset,
                           Judgment &sp_dead,
                           Judgment &init_subset);

    Judgment apply_rule_ci(Judgment &init_dead);
    Judgment apply_rule_cg(Judgment &goal_dead);

    Judgment apply_rule_ur(SetExpression left_set, SetExpression right_set);
    Judgment apply_rule_ul(SetExpression left_set, SetExpression right_set);
    Judgment apply_rule_ir(SetExpression left_set, SetExpression right_set);
    Judgment apply_rule_il(SetExpression left_set, SetExpression right_set);
    Judgment apply_rule_di(SetExpression left_set, SetExpression right_set);
    Judgment apply_rule_su(SetExpression left_set, SetExpression right_set,
                           Judgment &e_subset_epp,
                           Judgment &ep_subset_epp);
    Judgment apply_rule_si(SetExpression left_set, SetExpression right_set,
                           Judgment &e_subset_ep,
                           Judgment &e_subset_epp);
    Judgment apply_rule_st(SetExpression left_set, SetExpression right_set,
                           Judgment &e_subset_ep,
                           Judgment &ep_subset_epp);

    Judgment apply_rule_at(SetExpression left_set, SetExpression right_set,
                           Judgment &prog_subset,
                           Judgment &a_subset);
    Judgment apply_rule_au(SetExpression left_set, SetExpression right_set,
                           Judgment &a_prog_subset,
                           Judgment &ap_prog_subset);
    Judgment apply_rule_pt(SetExpression left_set, SetExpression right_set,
                           Judgment &prog_subset,
                           Judgment &sp_subset_s);
    Judgment apply_rule_pu(SetExpression left_set, SetExpression right_set,
                           Judgment &s_prog_subset,
                           Judgment &sp_prog_subset);
    Judgment apply_rule_pr(SetExpression left_set, SetExpression right_set,
                           Judgment &progression);
    Judgment apply_rule_rp(SetExpression left_set, SetExpression right_set,
                           Judgment &regression);
};

#endif // CERTIFICATEMANAGER_H
