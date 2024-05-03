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

#include "instructions.h"
#include "utils.h"
#include "memory_subsystem.h"
#include "backend.h"
#include "frontend.h"

using namespace std;





class CPU;


using namespace std;

struct CPU_Config
{
    uint32_t cpu_frequency_Hz = 1;
    // the total available memory in 'ints' the CPU can use.
    uint32_t memory_size = 16;
    // the number of architectural registers
    uint16_t arch_reg_count = 16;
    // the number of physical registers
    uint16_t phys_reg_count = 64;
    // true if every instruction execution should be printed
    bool trace = false;
    // the capacity of the store buffer
    uint16_t sb_capacity = 4;
    // the capacity of the instruction queue between frontend and backend
    uint8_t instr_queue_capacity = 16;

    uint8_t rob_capacity = 16;

    uint16_t rs_count = 1;
};


class CPU
{

private:

    bool is_idle();

    void cycle();

public:
    uint64_t cycles = 0;
    vector<int> *arch_regs;
    vector<int> *phys_regs;
    vector<int> *memory;
    InstrQueue instr_queue;
    StoreBuffer sb;
    chrono::milliseconds cycle_period_ms;
    Frontend frontend;
    Backend backend;

    CPU(CPU_Config config)
    {
        arch_regs = new vector<int>();
        for (int k = 0; k < config.arch_reg_count; k++)
        {
            arch_regs->push_back(0);
        }

        phys_regs = new vector<int>();
        for (int k = 0; k < config.phys_reg_count; k++)
        {
            phys_regs->push_back(0);
        }

        memory = new vector<int>();
        for (int k = 0; k < config.memory_size; k++)
        {
            memory->push_back(0);
        }

        double pause = 1.0 / config.cpu_frequency_Hz;
        cycle_period_ms = chrono::milliseconds(static_cast<int>(pause * 1000));

        sb.entries = new StoreBufferEntry[config.sb_capacity];
        sb.capacity = config.sb_capacity;
        sb.memory = memory;

        instr_queue.capacity = config.instr_queue_capacity;
        instr_queue.head = 0;
        instr_queue.tail = 0;
        instr_queue.entries = new Instr *[config.instr_queue_capacity];

        frontend.code = new vector<Instr>();
        frontend.ip_next_fetch = -1;
        frontend.bubble_remain = 0;
        frontend.nop = new Instr();
        frontend.nop->opcode = OPCODE_NOP;
        frontend.bubble_remain = 0;
        frontend.instr_queue = &instr_queue;

        backend.frontend = &frontend;
        backend.trace = config.trace;
        backend.arch_regs = arch_regs;
        backend.sb = &sb;
        backend.memory = memory;
        backend.instr_queue = &instr_queue;
        backend.rob.head = 0;
        backend.rob.tail = 0;
        backend.rob.reserved = 0;
        backend.rob.capacity = config.rob_capacity;
        backend.rob.slots = new ROB_Slot[config.rob_capacity];

        backend.eu.sb = &sb;
        backend.eu.backend = &backend;
        backend.eu.arch_regs = arch_regs;
        backend.eu.memory = memory;

        backend.rs_count = config.rs_count;
        backend.rs_array = new RS[config.rs_count];
        for (uint16_t k = 0; k < config.rs_count; k++)
        {
            RS &rs = backend.rs_array[k];
            rs.backend = &backend;
            rs.rs_index = k;
        }
        backend.rs_free_count = config.rs_count;
        backend.rs_free_array = new uint16_t[config.rs_count];
        for (uint16_t k = 0; k < config.rs_count; k++)
        {
            backend.rs_free_array[k] = k;
        }

        backend.rs_ready_head = 0;
        backend.rs_ready_tail = 0;
        backend.rs_ready_array = new uint16_t[config.rs_count];
    }

    /**
     * Runs the program till completion (including writing the store buffer to memory).
     */
    void run();

    void print_memory() const;

};

#endif //CPU_EMULATOR_CPU_H
