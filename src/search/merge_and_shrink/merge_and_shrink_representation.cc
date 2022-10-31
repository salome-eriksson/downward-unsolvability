#include "merge_and_shrink_representation.h"

#include "distances.h"
#include "types.h"

#include "../task_proxy.h"

#include "../utils/logging.h"

#include <algorithm>
#include <cassert>
#include <iostream>
#include <numeric>

using namespace std;

namespace merge_and_shrink {
MergeAndShrinkRepresentation::MergeAndShrinkRepresentation(int domain_size)
    : domain_size(domain_size) {
}

MergeAndShrinkRepresentation::~MergeAndShrinkRepresentation() {
}

int MergeAndShrinkRepresentation::get_domain_size() const {
    return domain_size;
}


MergeAndShrinkRepresentationLeaf::MergeAndShrinkRepresentationLeaf(
    int var_id, int domain_size)
    : MergeAndShrinkRepresentation(domain_size),
      var_id(var_id),
      lookup_table(domain_size) {
    iota(lookup_table.begin(), lookup_table.end(), 0);
}

void MergeAndShrinkRepresentationLeaf::set_distances(
    const Distances &distances) {
    assert(distances.are_goal_distances_computed());
    for (int &entry : lookup_table) {
        if (entry != PRUNED_STATE) {
            entry = distances.get_goal_distance(entry);
        }
    }
}

void MergeAndShrinkRepresentationLeaf::apply_abstraction_to_lookup_table(
    const vector<int> &abstraction_mapping) {
    int new_domain_size = 0;
    for (int &entry : lookup_table) {
        if (entry != PRUNED_STATE) {
            entry = abstraction_mapping[entry];
            new_domain_size = max(new_domain_size, entry + 1);
        }
    }
    domain_size = new_domain_size;
}

int MergeAndShrinkRepresentationLeaf::get_value(const State &state) const {
    int value = state[var_id].get_value();
    return lookup_table[value];
}

bool MergeAndShrinkRepresentationLeaf::is_total() const {
    for (int entry : lookup_table) {
        if (entry == PRUNED_STATE) {
            return false;
        }
    }
    return true;
}

void MergeAndShrinkRepresentationLeaf::dump(utils::LogProxy &log) const {
    log << "lookup table (leaf): ";
    for (const auto &value : lookup_table) {
        log << value << ", ";
    }
    log << endl;
}

void MergeAndShrinkRepresentationLeaf::get_bdds(
        CuddManager *manager, std::unordered_map<int, CuddBDD> &bdd_for_val) {

    CuddBDD mutexbdd = CuddBDD(manager,false);

    std::vector<std::pair<int,int>> pos;
    std::vector<std::pair<int,int>> neg;
    for(size_t i = 1; i < lookup_table.size(); ++i) {
        neg.push_back(std::make_pair(var_id, i));
    }
    pos.push_back(std::make_pair(var_id, 0));

    CuddBDD varbdd = CuddBDD(manager, pos, neg);
    bdd_for_val.insert({lookup_table[0],varbdd});
    mutexbdd.lor(varbdd);
    for(size_t i = 1; i < lookup_table.size(); ++i) {
        neg[i-1].second = i-1;
        pos[0].second = i;
        varbdd = CuddBDD(manager, pos, neg);
        if(bdd_for_val.find(lookup_table[i]) == bdd_for_val.end()) {
            bdd_for_val.insert({lookup_table[i],varbdd});
        } else {
            bdd_for_val[lookup_table[i]].lor(varbdd);
        }
        mutexbdd.lor(varbdd);
    }
    bdd_for_val.insert({-2, mutexbdd});
}

CuddBDD *MergeAndShrinkRepresentationLeaf::get_deadend_bdd(
            CuddManager *manager, std::unordered_map<int, CuddBDD> &bdd_for_val, bool first) {

    CuddBDD* b_inf = new CuddBDD(manager, false);

    std::vector<std::pair<int,int>> pos;
    std::vector<std::pair<int,int>> neg;
    for(size_t i = 1; i < lookup_table.size(); ++i) {
        neg.push_back(std::make_pair(var_id, i));
    }
    pos.push_back(std::make_pair(var_id, 0));

    int val = lookup_table[0];
    CuddBDD varbdd = CuddBDD(manager, pos, neg);
    varbdd.land(bdd_for_val[val]);
    b_inf->lor(varbdd);

    for(size_t i = 1; i < lookup_table.size(); ++i) {
        val = lookup_table[i];
        neg[i-1].second = i-1;
        pos[0].second = i;
        if(first && val >= 0) {
            val = 0;
        }
        varbdd = CuddBDD(manager, pos, neg);
        varbdd.land(bdd_for_val[val]);
        b_inf->lor(varbdd);
    }
    return b_inf;
}

void MergeAndShrinkRepresentationLeaf::fill_varorder(std::vector<int> &varorder) {
    varorder.push_back(var_id);
}

MergeAndShrinkRepresentationMerge::MergeAndShrinkRepresentationMerge(
    unique_ptr<MergeAndShrinkRepresentation> left_child_,
    unique_ptr<MergeAndShrinkRepresentation> right_child_)
    : MergeAndShrinkRepresentation(left_child_->get_domain_size() *
                                   right_child_->get_domain_size()),
      left_child(move(left_child_)),
      right_child(move(right_child_)),
      lookup_table(left_child->get_domain_size(),
                   vector<int>(right_child->get_domain_size())) {
    int counter = 0;
    for (vector<int> &row : lookup_table) {
        for (int &entry : row) {
            entry = counter;
            ++counter;
        }
    }
}

void MergeAndShrinkRepresentationMerge::set_distances(
    const Distances &distances) {
    assert(distances.are_goal_distances_computed());
    for (vector<int> &row : lookup_table) {
        for (int &entry : row) {
            if (entry != PRUNED_STATE) {
                entry = distances.get_goal_distance(entry);
            }
        }
    }
}

void MergeAndShrinkRepresentationMerge::apply_abstraction_to_lookup_table(
    const vector<int> &abstraction_mapping) {
    int new_domain_size = 0;
    for (vector<int> &row : lookup_table) {
        for (int &entry : row) {
            if (entry != PRUNED_STATE) {
                entry = abstraction_mapping[entry];
                new_domain_size = max(new_domain_size, entry + 1);
            }
        }
    }
    domain_size = new_domain_size;
}

int MergeAndShrinkRepresentationMerge::get_value(
    const State &state) const {
    int state1 = left_child->get_value(state);
    int state2 = right_child->get_value(state);
    if (state1 == PRUNED_STATE || state2 == PRUNED_STATE)
        return PRUNED_STATE;
    return lookup_table[state1][state2];
}

bool MergeAndShrinkRepresentationMerge::is_total() const {
    for (const vector<int> &row : lookup_table) {
        for (int entry : row) {
            if (entry == PRUNED_STATE) {
                return false;
            }
        }
    }
    return left_child->is_total() && right_child->is_total();
}

void MergeAndShrinkRepresentationMerge::dump(utils::LogProxy &log) const {
    log << "lookup table (merge): " << endl;
    for (const auto &row : lookup_table) {
        for (const auto &value : row) {
            log << value << ", ";
        }
        log << endl;
    }
    log << "left child:" << endl;
    left_child->dump(log);
    log << "right child:" << endl;
    right_child->dump(log);
}

void MergeAndShrinkRepresentationMerge::get_bdds(
        CuddManager *, std::unordered_map<int, CuddBDD> &) {
    // TODO: clearer error message
    std::cerr << "Non-linear merge strategy";
    exit(1);
}


CuddBDD* MergeAndShrinkRepresentationMerge::get_deadend_bdd(
         CuddManager * manager, std::unordered_map<int,CuddBDD> &bdd_for_val, bool first) {
    size_t rows = lookup_table.size();
    size_t columns = lookup_table[0].size();

    std::unordered_map<int, CuddBDD> row_bdds;

    // for linear merge strategies, the right child is always an atomic abstraction
    std::unordered_map<int, CuddBDD> right_child_bdds;
    right_child->get_bdds(manager, right_child_bdds);


    int val;
    for(size_t i = 0; i < rows; ++i) {
        CuddBDD b_i = CuddBDD(manager, false);
        for(size_t j = 0; j < columns; ++j) {
            val = lookup_table[i][j];
            if(first && val >= 0) {
                val = 0;
            }
            CuddBDD right_child = right_child_bdds[j];
            right_child.land(bdd_for_val[val]);
            b_i.lor(right_child);
        }
        if(right_child_bdds.find(-1) != right_child_bdds.end()) {
            CuddBDD right_child = right_child_bdds[-1];
            right_child.land(bdd_for_val[-1]);
            b_i.lor(right_child);
        }
        row_bdds.insert({i, b_i});
    }
    // TODO: Hack: -2 is the mutex BDD
    CuddBDD mutex_bdd = right_child_bdds[-2];
    mutex_bdd.land(bdd_for_val[-1]);
    row_bdds.insert({-1, mutex_bdd});

    return left_child->get_deadend_bdd(manager, row_bdds, false);
}

void MergeAndShrinkRepresentationMerge::fill_varorder(std::vector<int> &varorder) {
    // TODO: doublecheck if we get the varorder right!
    left_child->fill_varorder(varorder);
    right_child->fill_varorder(varorder);
}
}
