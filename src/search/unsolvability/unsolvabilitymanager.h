#ifndef UNSOLVABILITYMANAGER_H
#define UNSOLVABILITYMANAGER_H

#include "../abstract_task.h"
#include "../task_proxy.h"

#include <fstream>


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
};

#endif // UNSOLVABILITYMANAGER_H
