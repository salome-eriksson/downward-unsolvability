#include "best_first_open_list.h"

#include "../evaluator.h"
#include "../open_list.h"

#include "../plugins/plugin.h"
#include "../utils/memory.h"

#include <cassert>
#include <deque>
#include <map>

using namespace std;

namespace standard_scalar_open_list {
template<class Entry>
class BestFirstOpenList : public OpenList<Entry> {
    typedef deque<Entry> Bucket;

    map<int, Bucket> buckets;
    int size;

    shared_ptr<Evaluator> evaluator;

protected:
    virtual void do_insertion(EvaluationContext &eval_context,
                              const Entry &entry) override;

public:
    explicit BestFirstOpenList(const plugins::Options &opts);
    BestFirstOpenList(const shared_ptr<Evaluator> &eval, bool preferred_only);
    virtual ~BestFirstOpenList() override = default;

    virtual Entry remove_min() override;
    virtual bool empty() const override;
    virtual void clear() override;
    virtual void get_path_dependent_evaluators(set<Evaluator *> &evals) override;
    virtual bool is_dead_end(
        EvaluationContext &eval_context) const override;
    virtual bool is_reliable_dead_end(
        EvaluationContext &eval_context) const override;

    virtual int create_subcertificate(EvaluationContext &eval_context) override;
    virtual void write_subcertificates(const std::string &filename) override;
    virtual std::vector<int> get_varorder() override;

    virtual void store_deadend_info(EvaluationContext &eval_context) override;
    virtual std::pair<SetExpression,Judgment> get_dead_end_justification(
            EvaluationContext &eval_context, UnsolvabilityManager &unsolvmanager) override;
};


template<class Entry>
BestFirstOpenList<Entry>::BestFirstOpenList(const plugins::Options &opts)
    : OpenList<Entry>(opts.get<bool>("pref_only")),
      size(0),
      evaluator(opts.get<shared_ptr<Evaluator>>("eval")) {
}

template<class Entry>
BestFirstOpenList<Entry>::BestFirstOpenList(
    const shared_ptr<Evaluator> &evaluator, bool preferred_only)
    : OpenList<Entry>(preferred_only),
      size(0),
      evaluator(evaluator) {
}

template<class Entry>
void BestFirstOpenList<Entry>::do_insertion(
    EvaluationContext &eval_context, const Entry &entry) {
    int key = eval_context.get_evaluator_value(evaluator.get());
    buckets[key].push_back(entry);
    ++size;
}

template<class Entry>
Entry BestFirstOpenList<Entry>::remove_min() {
    assert(size > 0);
    auto it = buckets.begin();
    assert(it != buckets.end());
    Bucket &bucket = it->second;
    assert(!bucket.empty());
    Entry result = bucket.front();
    bucket.pop_front();
    if (bucket.empty())
        buckets.erase(it);
    --size;
    return result;
}

template<class Entry>
bool BestFirstOpenList<Entry>::empty() const {
    return size == 0;
}

template<class Entry>
void BestFirstOpenList<Entry>::clear() {
    buckets.clear();
    size = 0;
}

template<class Entry>
void BestFirstOpenList<Entry>::get_path_dependent_evaluators(
    set<Evaluator *> &evals) {
    evaluator->get_path_dependent_evaluators(evals);
}

template<class Entry>
bool BestFirstOpenList<Entry>::is_dead_end(
    EvaluationContext &eval_context) const {
    return eval_context.is_evaluator_value_infinite(evaluator.get());
}

template<class Entry>
bool BestFirstOpenList<Entry>::is_reliable_dead_end(
    EvaluationContext &eval_context) const {
    return is_dead_end(eval_context) && evaluator->dead_ends_are_reliable();
}

template<class Entry>
int BestFirstOpenList<Entry>::create_subcertificate(EvaluationContext &eval_context) {
    if (eval_context.is_evaluator_value_infinite(evaluator.get())) {
        return evaluator->create_subcertificate(eval_context);
    }
    std::cerr << "Requested subcertificate for non-dead state." << std::endl;
    utils::exit_with(utils::ExitCode::SEARCH_CRITICAL_ERROR);
}

template<class Entry>
void BestFirstOpenList<Entry>::write_subcertificates(const std::string &filename) {
    evaluator->write_subcertificates(filename);
}

template<class Entry>
std::vector<int> BestFirstOpenList<Entry>::get_varorder() {
    return evaluator->get_varorder();
}

template<class Entry>
void BestFirstOpenList<Entry>::store_deadend_info(EvaluationContext &eval_context) {
    if (eval_context.is_evaluator_value_infinite(evaluator.get())) {
        evaluator->store_deadend_info(eval_context);
    }
}

template<class Entry>
std::pair<SetExpression,Judgment> BestFirstOpenList<Entry>::get_dead_end_justification(
        EvaluationContext &eval_context, UnsolvabilityManager &unsolvmanager) {
    if (eval_context.is_evaluator_value_infinite(evaluator.get())) {
        return evaluator->get_dead_end_justification(eval_context, unsolvmanager);
    }
    std::cerr << "Requested proof of deadness for non-dead state." << std::endl;
    utils::exit_with(utils::ExitCode::SEARCH_CRITICAL_ERROR);
}

BestFirstOpenListFactory::BestFirstOpenListFactory(
    const plugins::Options &options)
    : options(options) {
}

unique_ptr<StateOpenList>
BestFirstOpenListFactory::create_state_open_list() {
    return utils::make_unique_ptr<BestFirstOpenList<StateOpenListEntry>>(options);
}

unique_ptr<EdgeOpenList>
BestFirstOpenListFactory::create_edge_open_list() {
    return utils::make_unique_ptr<BestFirstOpenList<EdgeOpenListEntry>>(options);
}

class BestFirstOpenListFeature : public plugins::TypedFeature<OpenListFactory, BestFirstOpenListFactory> {
public:
    BestFirstOpenListFeature() : TypedFeature("single") {
        document_title("Best-first open list");
        document_synopsis(
            "Open list that uses a single evaluator and FIFO tiebreaking.");

        add_option<shared_ptr<Evaluator>>("eval", "evaluator");
        add_option<bool>(
            "pref_only",
            "insert only nodes generated by preferred operators", "false");

        document_note(
            "Implementation Notes",
            "Elements with the same evaluator value are stored in double-ended "
            "queues, called \"buckets\". The open list stores a map from evaluator "
            "values to buckets. Pushing and popping from a bucket runs in constant "
            "time. Therefore, inserting and removing an entry from the open list "
            "takes time O(log(n)), where n is the number of buckets.");
    }
};

static plugins::FeaturePlugin<BestFirstOpenListFeature> _plugin;
}
