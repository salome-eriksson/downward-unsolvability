#ifndef MERGE_AND_SHRINK_MERGE_AND_SHRINK_HEURISTIC_H
#define MERGE_AND_SHRINK_MERGE_AND_SHRINK_HEURISTIC_H

#include "../heuristic.h"
#include "../evaluation_context.h"
#include "../unsolvability/cudd_interface.h"

#include <memory>

namespace merge_and_shrink {
class FactoredTransitionSystem;
class MergeAndShrinkRepresentation;

class MergeAndShrinkHeuristic : public Heuristic {
    // The final merge-and-shrink representations, storing goal distances.
    std::vector<std::unique_ptr<MergeAndShrinkRepresentation>> mas_representations;

    void extract_factor(FactoredTransitionSystem &fts, int index);
    bool extract_unsolvable_factor(FactoredTransitionSystem &fts);
    void extract_nontrivial_factors(FactoredTransitionSystem &fts);
    void extract_factors(FactoredTransitionSystem &fts);

    CuddManager *cudd_manager;
    std::vector<int> variable_order;
    // TODO: does this have to be a raw pointer?
    CuddBDD *bdd;
    std::string bdd_filename;
    std::pair<SetExpression, Judgment> set_and_dead_knowledge;
    bool deadends_shown_dead;

    void get_bdd();
protected:
    virtual int compute_heuristic(const State &ancestor_state) override;
public:
    explicit MergeAndShrinkHeuristic(const plugins::Options &opts);

    // currently not used
    //virtual void store_deadend_info(EvaluationContext &eval_context) override;
    virtual std::pair<SetExpression, Judgment> get_dead_end_justification(
        EvaluationContext &eval_context, UnsolvabilityManager &unsolvmanager) override;
};
}

#endif
