#ifndef UNSOLVABILITYMANAGER_H
#define UNSOLVABILITYMANAGER_H

#include "../abstract_task.h"
#include "../task_proxy.h"

#include <fstream>

class Judgment {
    friend class UnsolvabilityManager;
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

class UnsolvabilityManager
{
private:
    std::shared_ptr<AbstractTask> task;
    TaskProxy task_proxy;

    int setcount;
    int knowledgecount;

    int emptysetid;
    int goalsetid;
    int initsetid;
    int k_empty_dead;

    std::ofstream certstream;

    std::string directory;
    std::vector<char> hex;

    int apply_dead_rule(int setid, std::string rulename, std::vector<int> justification);
    int apply_subset_rule(int left_setid, int right_setid, std::string rulename, std::vector<int> justification);

public:
    UnsolvabilityManager(std::string directory, std::shared_ptr<AbstractTask> task);

    int get_new_setid();
    int get_new_knowledgeid();

    int get_emptysetid();
    int get_goalsetid();
    int get_initsetid();
    int get_k_empty_dead();

    std::ofstream &get_stream();

    std::string &get_directory();

    void dump_state(const State &state);

    // TODO: should we return only const Judgments?`
    // TODO: have sets instead of setids

    Judgment make_statement(int left_setid, int right_setid, std::string type);

    Judgment apply_rule_ed(); // TODO: only apply this once and return the proper Judgment on subsequent calls
    Judgment apply_rule_ud(int setid, Judgment &s_dead, Judgment &sp_dead);
    // TODO: in the thesis, the judgments for SD are the other way around
    Judgment apply_rule_sd(int setid, Judgment &s_subset_sp, Judgment &sp_dead);
    Judgment apply_rule_pg(int setid, Judgment &progression_subset,
                                 Judgment &sp_dead,
                                 Judgment &goal_intersection_dead);
    Judgment apply_rule_pi(int setid, Judgment &progression_subset,
                                 Judgment &sp_dead,
                                 Judgment &init_subset);
    Judgment apply_rule_rg(int setid, Judgment &regression_subset,
                                 Judgment &sp_dead,
                                 Judgment &goal_intersection_dead);
    Judgment apply_rule_ri(int setid, Judgment &regression_subset,
                                 Judgment &sp_dead,
                                 Judgment &init_subset);

    Judgment apply_rule_ci(Judgment &init_dead);
    Judgment apply_rule_cg(Judgment &goal_dead);

    Judgment apply_rule_ur(int left_setid, int right_setid);
    Judgment apply_rule_ul(int left_setid, int right_setid);
    Judgment apply_rule_ir(int left_setid, int right_setid);
    Judgment apply_rule_il(int left_setid, int right_setid);
    Judgment apply_rule_di(int left_setid, int right_setid);
    Judgment apply_rule_su(int left_setid, int right_setid,
                                 Judgment &e_subset_epp,
                                 Judgment &ep_subset_epp);
    Judgment apply_rule_si(int left_setid, int right_setid,
                                 Judgment &e_subset_ep,
                                 Judgment &e_subset_epp);
    Judgment apply_rule_st(int left_setid, int right_setid,
                                 Judgment &e_subset_ep,
                                 Judgment &ep_subset_epp);

    Judgment apply_rule_at(int left_setid, int right_setid,
                                 Judgment &prog_subset,
                                 Judgment &a_subset);
    Judgment apply_rule_au(int left_setid, int right_setid,
                                 Judgment &a_prog_subset,
                                 Judgment &ap_prog_subset);
    Judgment apply_rule_pt(int left_setid, int right_setid,
                                 Judgment &prog_subset,
                                 Judgment &sp_subset_s);
    Judgment apply_rule_pu(int left_setid, int right_setid,
                                 Judgment &s_prog_subset,
                                 Judgment &sp_prog_subset);
    Judgment apply_rule_pr(int left_setid, int right_setid,
                                 Judgment &progression);
    Judgment apply_rule_rp(int left_setid, int right_setid,
                                 Judgment &regression);

};

#endif // UNSOLVABILITYMANAGER_H
