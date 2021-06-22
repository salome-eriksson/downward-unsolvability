#include "unsolvabilitymanager.h"

#include "../utils/system.h"
#include "../task_proxy.h"


UnsolvabilityManager::UnsolvabilityManager(
        std::string directory, std::shared_ptr<AbstractTask> task)
    : task(task), task_proxy(*task), setcount(0), knowledgecount(0), directory(directory) {
    certstream.open(directory + "proof.txt");

    emptysetid = setcount++;
    certstream << "e " << emptysetid << " c e\n";
    goalsetid = setcount++;
    certstream << "e " << goalsetid << " c g\n";
    initsetid = setcount++;
    certstream << "e " << initsetid << " c i\n";
    k_empty_dead = knowledgecount++;
    certstream << "k " << k_empty_dead << " d " << emptysetid << " ed\n";
    certstream << "a 0 a\n";

    hex = {'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e' , 'f'};
}

int UnsolvabilityManager::get_new_setid() {
    return setcount++;
}
int UnsolvabilityManager::get_new_knowledgeid() {
    return knowledgecount++;
}

int UnsolvabilityManager::get_emptysetid() {
    return emptysetid;
}
int UnsolvabilityManager::get_goalsetid() {
    return goalsetid;
}
int UnsolvabilityManager::get_initsetid() {
    return initsetid;
}
int UnsolvabilityManager::get_k_empty_dead() {
    return k_empty_dead;
}

std::ofstream &UnsolvabilityManager::get_stream() {
    return certstream;
}

std::string &UnsolvabilityManager::get_directory() {
    return directory;
}


void UnsolvabilityManager::dump_state(const State &state) {
    int c = 0;
    int count = 3;
    for(FactProxy fact : state) {
        for(int j = 0; j < fact.get_variable().get_domain_size(); ++j) {
            if(fact.get_value() == j) {
                c += (1 << count);
            }
            count--;
            if(count==-1) {
                certstream << hex[c];
                c = 0;
                count = 3;
            }
        }
    }
    if(count != 3) {
        certstream << hex[c];
    }
}
