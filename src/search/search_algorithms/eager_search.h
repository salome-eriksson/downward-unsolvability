#ifndef SEARCH_ALGORITHMS_EAGER_SEARCH_H
#define SEARCH_ALGORITHMS_EAGER_SEARCH_H

#include "../open_list.h"
#include "../search_algorithm.h"

#include "../certificates/cudd_interface.h"

#include <memory>
#include <vector>

class Evaluator;
class PruningMethod;

namespace plugins {
class Feature;
}

namespace eager_search {
class EagerSearch : public SearchAlgorithm {
    const bool reopen_closed_nodes;
    const UnsolvabilityVerificationType unsolv_type;

    std::unique_ptr<StateOpenList> open_list;
    std::shared_ptr<Evaluator> f_evaluator;

    std::vector<Evaluator *> path_dependent_evaluators;
    std::vector<std::shared_ptr<Evaluator>> preferred_operator_evaluators;
    std::shared_ptr<Evaluator> lazy_evaluator;

    std::shared_ptr<PruningMethod> pruning_method;

    void start_f_value_statistics(EvaluationContext &eval_context);
    void update_f_value_statistics(EvaluationContext &eval_context);
    void reward_progress();

    std::string certificate_directory;
    std::ofstream unsolvability_certificate_hints;

protected:
    virtual void initialize() override;
    virtual SearchStatus step() override;

public:
    explicit EagerSearch(const plugins::Options &opts);
    virtual ~EagerSearch() = default;

    virtual void print_statistics() const override;

    void dump_search_space() const;

    void write_unsolvability_certificate();
    void write_unsolvability_proof();
    void write_certificate_task_file(const std::vector<int> &varorder);
};

extern void add_options_to_feature(plugins::Feature &feature);
}

#endif
