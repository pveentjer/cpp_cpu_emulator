//
// Created by pveentjer on 4/30/24.
//

#ifndef CPU_EMULATOR_CPU_H
#define CPU_EMULATOR_CPU_H

#include <iostream>
#include <vector>
#include <thread>
#include <optional>
#include <map>

#include "common.h"
#include "instructions.h"
#include "utils.h"
#include "memory_subsystem.h"
#include "backend.h"
#include "frontend.h"

using namespace std;


class CPU;




class CPU
{

private:

    bool is_idle();

    void cycle();

public:
    uint64_t cycles = 0;
    vector<int> *memory;
    InstrQueue* instr_queue;
    StoreBuffer *sb;
    chrono::milliseconds cycle_period_ms;
    Frontend* frontend;
    Backend* backend;

    CPU(CPU_Config config)
    {

        memory = new vector<int>();
        for (int k = 0; k < config.memory_size_ints; k++)
        {
            memory->push_back(0);
        }
        sb = new StoreBuffer(config.sb_capacity, memory);

        double pause = 1.0 / config.cpu_frequency_Hz;
        cycle_period_ms = chrono::milliseconds(static_cast<int>(pause * 1000));


        instr_queue = new InstrQueue(config.instr_queue_capacity);

        frontend = new Frontend(&config, instr_queue);
        backend = new Backend(config, frontend, instr_queue, memory, sb);
    }

    // todo: destructor

    /**
     * Runs the program till completion (including writing the store buffer to memory_addr).
     */
    void run();

    void print_memory() const;

};

#endif //CPU_EMULATOR_CPU_H
