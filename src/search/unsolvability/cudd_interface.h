#ifndef CUDD_INTERFACE_H
#define CUDD_INTERFACE_H


#include <functional>
#include <memory>
#include <vector>
#include <iostream>
#include <fstream>

#include "../abstract_task.h"
#include "../task_proxy.h"
#include "../utils/system.h"



/*
  All methods that use CUDD specific classes only do something useful
  if the planner is compiled with USE_CUDD. Otherwise, they just print
  an error message and abort.
*/
#ifdef HAS_CUDD
#include "cudd.h"
#define CUDD_METHOD(X) X;
#else
class DdNode {
};
#define CUDD_METHOD(X) NO_RETURN X { \
        ABORT("CUDD method called but the planner was compiled without CUDD support.\n"); \
}
#endif

class Cudd;

#ifdef __GNUG__
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-parameter"
#endif

class CuddManager;

class CuddBDD {
    friend class CuddManager;
private:
    CuddManager *manager;
    DdNode *bdd;
public:
    CUDD_METHOD(CuddBDD())
    CUDD_METHOD(CuddBDD(CuddManager *manager, bool positive = true))
    CUDD_METHOD(CuddBDD(CuddManager *manager, int var, int val, bool neg = false))
    CUDD_METHOD(CuddBDD(CuddManager *manager, const State &state))
    //create a cube where pos_facts are true and neg_facts are false
    CUDD_METHOD(CuddBDD(CuddManager *manager,
                        const std::vector<std::pair<int, int>> &pos_facts,
                        const std::vector<std::pair<int, int>> &neg_facts))
    CUDD_METHOD(CuddBDD(const CuddBDD &from))
    CUDD_METHOD(~CuddBDD())
    CUDD_METHOD(CuddBDD operator=(const CuddBDD &right))
    CUDD_METHOD(void land(int var, int val, bool neg = false))
    CUDD_METHOD(void lor(int var, int val, bool neg = false))
    CUDD_METHOD(void negate())
    CUDD_METHOD(void land(const CuddBDD &bdd2))
    CUDD_METHOD(void lor(const CuddBDD &bdd2))

    CUDD_METHOD(bool isOne() const)
    CUDD_METHOD(bool isZero() const)

    CUDD_METHOD(bool isEqualTo(const CuddBDD &bdd2) const)
    CUDD_METHOD(bool isSubsetOf(const CuddBDD &bdd2) const)

    //TODO: not sure if this is a good idea, it is used in heuristic representation in M&S
    //to get the M&S manager without needing to pass it through all the time
    CUDD_METHOD(CuddManager *get_manager())

    CUDD_METHOD(void dumpBDD(std::string filename, std::string bddname) const)
};

class CuddManager {
    friend class CuddBDD;
private:
#ifdef HAS_CUDD
    static bool compact_proof;
    DdManager *ddmgr;
    std::vector<int> var_order;
    std::vector<std::vector<int>> fact_to_var;
    std::string filename;

    // Hold a reference to the task implementation and pass it to objects that need it.
    const std::shared_ptr<AbstractTask> task;
    // Use task_proxy to access task information.
    TaskProxy task_proxy;
    int bdd_varamount;
#endif
public:
    // uses default var order
    CUDD_METHOD(CuddManager(std::shared_ptr<AbstractTask> task))
    CUDD_METHOD(CuddManager(std::shared_ptr<AbstractTask> task, std::vector<int> &var_order))
    CUDD_METHOD(const std::vector<std::vector<int>> *get_fact_to_var() const)
    CUDD_METHOD(void dumpBDDs_certificate(std::vector<CuddBDD> &bdds, std::vector<int> &indices, const std::string &filename) const)
    CUDD_METHOD(void dumpBDDs(std::vector<CuddBDD> &bdds, std::string directory) const)
    CUDD_METHOD(static void set_compact_proof(bool val))
    CUDD_METHOD(std::string get_filename() const)
};

#endif
