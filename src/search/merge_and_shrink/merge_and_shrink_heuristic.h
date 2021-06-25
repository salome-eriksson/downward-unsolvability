#ifndef MERGE_AND_SHRINK_MERGE_AND_SHRINK_HEURISTIC_H
#define MERGE_AND_SHRINK_MERGE_AND_SHRINK_HEURISTIC_H

#include "../heuristic.h"
#include "../evaluation_context.h"
#include "../unsolvability/cudd_interface.h"

#include <memory>

namespace utils {
enum class Verbosity;
}

namespace merge_and_shrink {
class FactoredTransitionSystem;
class MergeAndShrinkRepresentation;

class MergeAndShrinkHeuristic : public Heuristic {
    const utils::Verbosity verbosity;

    // The final merge-and-shrink representations, storing goal distances.
    std::vector<std::unique_ptr<MergeAndShrinkRepresentation>> mas_representations;

    void extract_factor(FactoredTransitionSystem &fts, int index);
    bool extract_unsolvable_factor(FactoredTransitionSystem &fts);
    void extract_nontrivial_factors(FactoredTransitionSystem &fts);
    void extract_factors(FactoredTransitionSystem &fts);

    CuddManager* cudd_manager;
    std::vector<int> variable_order;
    CuddBDD *bdd;

    int bdd_to_stateid;

    int setid;
    Judgment set_dead;
    std::string bdd_filename;

    void get_bdd();
protected:
    virtual int compute_heuristic(const State &ancestor_state) override;
public:
    explicit MergeAndShrinkHeuristic(const options::Options &opts);

    // currently not used
    //virtual void store_deadend_info(EvaluationContext &eval_context) override;
    virtual std::pair<int,Judgment> get_setid_and_deadjudment(
            EvaluationContext &eval_context, UnsolvabilityManager &unsolvmanager) override;
};
}

#endif
