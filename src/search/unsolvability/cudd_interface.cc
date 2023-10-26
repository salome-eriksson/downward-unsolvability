#include "cudd_interface.h"
#include "../utils/timer.h"

#include <algorithm>
#include <sstream>

#ifdef HAS_CUDD
#include "dddmp.h"

using utils::ExitCode;

void exit_oom(size_t size) {
    utils::exit_with(ExitCode::SEARCH_OUT_OF_MEMORY);
}

bool CuddManager::compact_proof = true;

CuddBDD::CuddBDD() : manager(NULL), bdd(NULL) {}

CuddBDD::CuddBDD(CuddManager *manager, bool positive)
    : manager(manager) {
    if (positive) {
        bdd = Cudd_ReadOne(manager->ddmgr);
    } else {
        bdd = Cudd_ReadLogicZero(manager->ddmgr);
    }
    Cudd_Ref(bdd);
}

CuddBDD::CuddBDD(CuddManager *manager, int var, int val, bool neg)
    : manager(manager) {
    assert(var < (int)manager->fact_to_var.size() && val < (int)manager->fact_to_var[var].size());
    bdd = Cudd_bddIthVar(manager->ddmgr, manager->fact_to_var[var][val]);
    if (neg) {
        bdd = Cudd_Not(bdd);
    }
    Cudd_Ref(bdd);
}

CuddBDD::CuddBDD(CuddManager *manager, const State &state)
    : manager(manager) {
    std::vector<int> cube(manager->bdd_varamount, 0);
    for (FactProxy fact : state) {
        cube[manager->fact_to_var[fact.get_variable().get_id()][fact.get_value()]] = 1;
    }
    bdd = Cudd_CubeArrayToBdd(manager->ddmgr, &cube[0]);
    Cudd_Ref(bdd);
}

CuddBDD::CuddBDD(CuddManager *manager, const std::vector<std::pair<int, int>> &pos_facts,
                 const std::vector<std::pair<int, int>> &neg_facts)
    : manager(manager) {
    std::vector<int> cube(manager->bdd_varamount, 2);
    for (size_t i = 0; i < pos_facts.size(); ++i) {
        const std::pair<int, int> &p = pos_facts[i];
        assert(p.first < (int)manager->fact_to_var.size()
               && p.second < (int)manager->fact_to_var[p.first].size());
        cube[manager->fact_to_var[p.first][p.second]] = 1;
    }
    for (size_t i = 0; i < neg_facts.size(); ++i) {
        const std::pair<int, int> &p = neg_facts[i];
        assert(p.first < (int)manager->fact_to_var.size()
               && p.second < (int)manager->fact_to_var[p.first].size());
        cube[manager->fact_to_var[p.first][p.second]] = 0;
    }
    bdd = Cudd_CubeArrayToBdd(manager->ddmgr, &cube[0]);
    Cudd_Ref(bdd);
}

CuddBDD::~CuddBDD() {
    Cudd_RecursiveDeref(manager->ddmgr, bdd);
}
//TODO: here and in operator= check why the manager pointer is not null sometimes
//(example: vector initialization with (amount, CuddBdd(manager, true)), concrete in heuristic_representation line 119)
CuddBDD::CuddBDD(const CuddBDD &from) {
    manager = from.manager;
    bdd = from.bdd;
    Cudd_Ref(bdd);
}

CuddBDD CuddBDD::operator=(const CuddBDD &right) {
    if (this == &right) {
        return *this;
    }
    manager = right.manager;
    Cudd_Ref(right.bdd);
    Cudd_RecursiveDeref(manager->ddmgr, bdd);
    bdd = right.bdd;
    return *this;
}

void CuddBDD::land(int var, int val, bool neg) {
    assert(var < (int)manager->fact_to_var.size() && val < (int)manager->fact_to_var[var].size());
    DdNode *tmp;
    if (neg) {
        tmp = Cudd_bddAnd(manager->ddmgr, bdd, Cudd_Not(Cudd_bddIthVar(manager->ddmgr, manager->fact_to_var[var][val])));
    } else {
        tmp = Cudd_bddAnd(manager->ddmgr, bdd, Cudd_bddIthVar(manager->ddmgr, manager->fact_to_var[var][val]));
    }
    Cudd_Ref(tmp);
    Cudd_RecursiveDeref(manager->ddmgr, bdd);
    bdd = tmp;
}

void CuddBDD::lor(int var, int val, bool neg) {
    assert(var < (int)manager->fact_to_var.size() && val < (int)manager->fact_to_var[var].size());
    DdNode *tmp;
    if (neg) {
        tmp = Cudd_bddOr(manager->ddmgr, bdd, Cudd_Not(Cudd_bddIthVar(manager->ddmgr, manager->fact_to_var[var][val])));
    } else {
        tmp = Cudd_bddOr(manager->ddmgr, bdd, Cudd_bddIthVar(manager->ddmgr, manager->fact_to_var[var][val]));
    }
    Cudd_Ref(tmp);
    Cudd_RecursiveDeref(manager->ddmgr, bdd);
    bdd = tmp;
}

void CuddBDD::negate() {
    DdNode *tmp = Cudd_Not(bdd);
    //TODO: is this necessary here?
    Cudd_Ref(tmp);
    Cudd_RecursiveDeref(manager->ddmgr, bdd);
    bdd = tmp;
}

void CuddBDD::land(const CuddBDD &bdd2) {
    DdNode *tmp = Cudd_bddAnd(manager->ddmgr, bdd, bdd2.bdd);
    Cudd_Ref(tmp);
    Cudd_RecursiveDeref(manager->ddmgr, bdd);
    bdd = tmp;
}

void CuddBDD::lor(const CuddBDD &bdd2) {
    DdNode *tmp = Cudd_bddOr(manager->ddmgr, bdd, bdd2.bdd);
    Cudd_Ref(tmp);
    Cudd_RecursiveDeref(manager->ddmgr, bdd);
    bdd = tmp;
}

bool CuddBDD::isOne() const {
    return bdd == Cudd_ReadOne(manager->ddmgr);
}

bool CuddBDD::isZero() const {
    return bdd == Cudd_ReadLogicZero(manager->ddmgr);
}

bool CuddBDD::isEqualTo(const CuddBDD &bdd2) const {
    return bdd == bdd2.bdd;
}

bool CuddBDD::isSubsetOf(const CuddBDD &bdd2) const {
    assert(manager->ddmgr == bdd2.manager->ddmgr);
    return Cudd_bddLeq(manager->ddmgr, bdd, bdd2.bdd) == 1;
}

CuddManager *CuddBDD::get_manager() {
    return manager;
}

void CuddBDD::dumpBDD(std::string filename, std::string bddname) const {
    FILE *f = fopen(filename.c_str(), "w");
    Dddmp_cuddBddStore(manager->ddmgr, &bddname[0], bdd, NULL, NULL,
                       DDDMP_MODE_TEXT, DDDMP_VARIDS, &filename[0], f);
    fclose(f);
}

CuddManager::CuddManager(std::shared_ptr<AbstractTask> task)
    : task(task), task_proxy(*task) {
    int varamount = task_proxy.get_variables().size();
    fact_to_var.resize(varamount, std::vector<int>());
    var_order.resize(varamount);
    bdd_varamount = 0;
    for (size_t i = 0; i < var_order.size(); ++i) {
        var_order[i] = i;
        int domsize = task_proxy.get_variables()[i].get_domain_size();
        fact_to_var[i].resize(domsize);
        for (int j = 0; j < domsize; ++j) {
            fact_to_var[i][j] = bdd_varamount++;
        }
    }
    ddmgr = Cudd_Init(bdd_varamount, 0, CUDD_UNIQUE_SLOTS, CUDD_CACHE_SLOTS, 0);
    Cudd_InstallOutOfMemoryHandler(exit_oom);
    Cudd_UnregisterOutOfMemoryCallback(ddmgr);

    // filename is based on the memory address of the manager to ensure uniqueness
    std::stringstream ss;
    ss << this << ".bdd";
    filename = ss.str();
}



CuddManager::CuddManager(std::shared_ptr<AbstractTask> task, std::vector<int> &var_order)
    : var_order(var_order), task(task), task_proxy(*task) {
    int varamount = task_proxy.get_variables().size();
    assert((int)var_order.size() == varamount);
    fact_to_var.resize(varamount, std::vector<int>());
    bdd_varamount = 0;
    for (size_t i = 0; i < var_order.size(); ++i) {
        int index = var_order[i];
        int domsize = task_proxy.get_variables()[index].get_domain_size();
        fact_to_var[index].resize(domsize);
        for (int j = 0; j < domsize; ++j) {
            fact_to_var[index][j] = bdd_varamount++;
        }
    }
    ddmgr = Cudd_Init(bdd_varamount, 0, CUDD_UNIQUE_SLOTS, CUDD_CACHE_SLOTS, 0);
    Cudd_InstallOutOfMemoryHandler(exit_oom);
    Cudd_UnregisterOutOfMemoryCallback(ddmgr);

    // filename is based on the memory address of the manager to ensure uniqueness
    std::stringstream ss;
    ss << this << ".bdd";
    filename = ss.str();
}

const std::vector<std::vector<int>> *CuddManager::get_fact_to_var() const {
    return &fact_to_var;
}

void CuddManager::dumpBDDs_certificate(std::vector<CuddBDD> &bdds, std::vector<int> &indices, const std::string &filename) const {
    int size = bdds.size();
    std::ofstream filestream;
    filestream.open(filename);
    if (size > 0) {
        filestream << size;
        DdNode **bdd_arr = new DdNode *[size];
        for (int i = 0; i < size; ++i) {
            bdd_arr[i] = bdds[i].bdd;
            filestream << " " << indices[i];
        }
        filestream << "\n";
        filestream.close();
        FILE *fp;
        fp = fopen(filename.c_str(), "a");
        Dddmp_cuddBddArrayStore(ddmgr, NULL, size, &bdd_arr[0], NULL,
                                NULL, NULL, DDDMP_MODE_TEXT, DDDMP_VARIDS, NULL, fp);
    }
    filestream.close();
}

void CuddManager::dumpBDDs(std::vector<CuddBDD> &bdds, std::string directory) const {
    std::ofstream stream;
    std::string full_filename = directory + filename;
    stream.open(full_filename);
    for (size_t i = 0; i < fact_to_var.size(); ++i) {
        for (size_t j = 0; j < fact_to_var[i].size(); ++j) {
            stream << fact_to_var[i][j] << " ";
        }
    }
    stream << "\n";
    stream.close();

    FILE *fp;
    fp = fopen(full_filename.c_str(), "a");

    if (compact_proof) {
        int size = bdds.size();
        DdNode **bdd_arr = new DdNode *[size];
        for (int i = 0; i < size; ++i) {
            fprintf(fp, "%d ", (int)i);
            bdd_arr[i] = bdds[i].bdd;
        }
        fprintf(fp, "\n");
        Dddmp_cuddBddArrayStore(ddmgr, NULL, size, &bdd_arr[0], NULL,
                                NULL, NULL, DDDMP_MODE_TEXT, DDDMP_VARIDS, NULL, fp);
    } else {
        DdNode **bdd_arr = new DdNode *[1];
        for (size_t i = 0; i < bdds.size(); ++i) {
            fprintf(fp, "%d\n", (int)i);
            bdd_arr[0] = bdds[i].bdd;
            Dddmp_cuddBddArrayStore(ddmgr, NULL, 1, &bdd_arr[0], NULL,
                                    NULL, NULL, DDDMP_MODE_TEXT, DDDMP_VARIDS, NULL, fp);
        }
    }
}

void CuddManager::set_compact_proof(bool val) {
    compact_proof = val;
}

std::string CuddManager::get_filename() const {
    return filename;
}

#endif
