#ifndef UNSOLVABILITYMANAGER_H
#define UNSOLVABILITYMANAGER_H

#include "cudd_interface.h"

#include "../abstract_task.h"
#include "../task_proxy.h"

#include <fstream>

class Judgment {
    friend class CertificateManager;
    size_t id;

    Judgment(size_t id)
        : id(id) {
    }

public:
    /*
     * TODO: ideally, I don't want to implement default construction
     * However, M&S needs this since it has a Judgment member
     */
    Judgment()
        : id(0) {
    }
    ~Judgment() {
    }
    bool operator==(const Judgment &other) const {
        return id == other.id;
    }
};

class SetExpression {
    friend class CertificateManager;
    size_t id;

    SetExpression(size_t id)
        : id(id) {

    }
public:
    /*
     * TODO: ideally, I don't want to implement default construction
     * However, eager_serach needs this since it has a SetExpression variable
     */
    SetExpression()
        : id(0) {
    }
    ~SetExpression() {
    }
    bool operator==(const SetExpression &other) const {
        return id == other.id;
    }
};

class CertificateManager
{
private:
    std::shared_ptr<AbstractTask> task;
    TaskProxy task_proxy;

    size_t actionset_count;
    size_t stateset_count;
    size_t knowledge_count;

    SetExpression emptyset;
    SetExpression goalset;
    SetExpression initset;
    SetExpression allactions;

    std::ofstream certstream;

    std::string directory;
    std::vector<char> hex;

    std::unordered_map<CuddManager *, std::vector<CuddBDD>> bdds;

    size_t get_new_actionsetid();
    size_t get_new_statesetid();
    size_t get_new_knowledgeid();

    size_t apply_bound_rule(size_t setid, size_t bound, std::string rulename, std::vector<size_t> justification);
    size_t apply_subset_rule(size_t left_setid, size_t right_setid, std::string rulename, std::vector<size_t> justification);

public:
    CertificateManager(std::string directory, std::shared_ptr<AbstractTask> task);


    const SetExpression &get_emptyset();
    const SetExpression &get_goalset();
    const SetExpression &get_initset();
    const SetExpression &get_allactions();

    void dump_state(const State &state);

    void dump_BDDs();

    // TODO: should we return only const Judgments and SetExpressions?

    SetExpression define_action(std::vector<int> action_indices);
    SetExpression define_action_set_union(const SetExpression &left_set,
                                          const SetExpression &right_set);

    // TODO: make BDD const (currently a problem when getting the manager)
    SetExpression define_bdd(CuddBDD bdd);
    SetExpression define_horn_formula(int varamount,
                                      std::vector<std::vector<int>> &clauses);
    SetExpression define_explicit_set(int fact_amount, StateRegistry &state_registry,
                                      std::vector<StateID> state_ids);

    SetExpression define_set_negation(const SetExpression &set);
    SetExpression define_set_union(const SetExpression &left_set,
                                   const SetExpression &right_set);
    SetExpression define_set_intersection(const SetExpression &left_set,
                                          const SetExpression &right_set);
    SetExpression define_set_progression(const SetExpression &set,
                                         const SetExpression &actionset);
    SetExpression define_set_regression(const SetExpression &set,
                                        const SetExpression &actionset);

    Judgment make_statement(const SetExpression &left_set,
                            const SetExpression &right_set, std::string type);

    Judgment apply_rule_ec();
    Judgment apply_rule_tc(const SetExpression set);
    Judgment apply_rule_sc(const SetExpression set, unsigned bound,
                           const Judgment &sp_bound, const Judgment &s_subset_sp);
    Judgment apply_rule_uc(const SetExpression set, unsigned bound,
                           const Judgment &s_bound, const Judgment &sp_bound);
    Judgment apply_rule_pc(const SetExpression set, unsigned bound,
                           const Judgment &actions_contained, const Judgment &nogoal,
                           std::vector<std::pair<Judgment,Judgment>> successor_bounds);

    Judgment apply_rule_bi(unsigned bound, const Judgment &init_bound);

    Judgment apply_rule_ur(const SetExpression &left_set, const SetExpression &right_set);
    Judgment apply_rule_ul(const SetExpression &left_set, const SetExpression &right_set);
    Judgment apply_rule_ir(const SetExpression &left_set, const SetExpression &right_set);
    Judgment apply_rule_il(const SetExpression &left_set, const SetExpression &right_set);
    Judgment apply_rule_di(const SetExpression &left_set, const SetExpression &right_set);
    Judgment apply_rule_su(const SetExpression &left_set, const SetExpression &right_set,
                                 const Judgment &e_subset_epp,
                                 const Judgment &ep_subset_epp);
    Judgment apply_rule_si(const SetExpression &left_set, const SetExpression &right_set,
                                 const Judgment &e_subset_ep,
                                 const Judgment &e_subset_epp);
    Judgment apply_rule_st(const SetExpression &left_set, const SetExpression &right_set,
                                 const Judgment &e_subset_ep,
                                 const Judgment &ep_subset_epp);

    Judgment apply_rule_at(const SetExpression &left_set, const SetExpression &right_set,
                                 const Judgment &prog_subset,
                                 const Judgment &a_subset);
    Judgment apply_rule_au(const SetExpression &left_set, const SetExpression &right_set,
                                 const Judgment &a_prog_subset,
                                 const Judgment &ap_prog_subset);
    Judgment apply_rule_pt(const SetExpression &left_set, const SetExpression &right_set,
                                 const Judgment &prog_subset,
                                 const Judgment &sp_subset_s);
    Judgment apply_rule_pu(const SetExpression &left_set, const SetExpression &right_set,
                                 const Judgment &s_prog_subset,
                                 const Judgment &sp_prog_subset);
    Judgment apply_rule_pr(const SetExpression &left_set, const SetExpression &right_set,
                                 const Judgment &progression);
    Judgment apply_rule_rp(const SetExpression &left_set, const SetExpression &right_set,
                                 const Judgment &regression);

};

#endif // UNSOLVABILITYMANAGER_H
