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

using namespace std;



// currently just 2 stages; fetch + decode
static const int PIPELINE_DEPTH = 2;

struct StoreBufferEntry
{
    int value;
    int addr;
};

struct StoreBuffer
{
    StoreBufferEntry *entries;
    uint16_t capacity;
    uint64_t head = 0;
    uint64_t tail = 0;
    vector<int> *memory;

    optional<int> lookup(int addr);

    bool is_empty();

    void write(int addr, int value);

    void cycle();
};

/**
 * The InstrQueue sits between frontend and backend.
 */
struct InstrQueue
{
    Instr **entries;
    uint16_t capacity;
    uint64_t head = 0;
    uint64_t tail = 0;

    bool is_empty() const
    {
        return head == tail;
    }

    uint16_t size() const
    {
        return tail - head;
    }

    bool is_full() const
    {
        return size() == capacity;
    }

    Instr *dequeue()
    {
        Instr *instr = entries[head % capacity];
        head++;
        return instr;
    }

    void enqueue(Instr *instr)
    {
        entries[tail % capacity] = instr;
        tail++;
    }
};

class CPU;

/**
 * The Frontend is responsible for fetching and decoding instruction
 * and then will place them on the InstrQueue for the backend.
 */
struct Frontend
{
    CPU *cpu;
    int bubble_size;
    int32_t ip_next_fetch = -1;
    Instr *nop;
    InstrQueue *instr_queue;

    void cycle();

    bool is_idle();

};

/**
 * The Backend is responsible for the actual execution of the instruction.
 *
 * It will take instructions from the InstrQueue.
 */
struct Backend
{
    CPU *cpu;
    // when true, prints every instruction before being executed.
    bool trace;
    vector<int> *arch_regs;
    StoreBuffer *sb;
    vector<int> *memory;
    InstrQueue *instr_queue;

    void execute(Instr *instr);

    void cycle();

    bool is_idle();
};

using namespace std;

struct CPU_Config
{
    uint32_t cpu_frequency_Hz = 1;
    // the total available memory in 'ints' the CPU can use.
    uint32_t memory_size = 16;
    // the number of architectural registers
    uint16_t arch_reg_count = 16;
    // true if every instruction execution should be printed
    bool trace = false;
    // the capacity of the store buffer
    uint16_t sb_capacity = 4;
    // the capacity of the instruction queue between frontend and backend
    uint8_t instr_queue_capacity = 16;
};

class CPU
{

private:

    bool is_idle();

    void cycle();

public:
    uint64_t cycles = 0;
    vector<Instr> *code;
    vector<int> *arch_regs;
    vector<int> *memory;
    InstrQueue instr_queue;
    StoreBuffer sb;
    chrono::milliseconds cycle_period_ms;
    Frontend frontend;
    Backend backend;

    CPU(CPU_Config config)
    {
        code = new vector<Instr>();
        arch_regs = new vector<int>();
        for (int k = 0; k < config.arch_reg_count; k++)
        {
            arch_regs->push_back(0);
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

        frontend.ip_next_fetch = -1;
        frontend.bubble_size = 0;
        frontend.nop = new Instr();
        frontend.nop->opcode = OPCODE_NOP;
        frontend.cpu = this;
        frontend.bubble_size = 0;
        frontend.instr_queue = &instr_queue;

        backend.cpu = this;
        backend.trace = config.trace;
        backend.arch_regs = arch_regs;
        backend.sb = &sb;
        backend.memory = memory;
        backend.instr_queue = &instr_queue;
    }

    /**
     * Runs the program till completion (including writing the store buffer to memory).
     */
    void run();

    void print_memory() const;

};

#endif //CPU_EMULATOR_CPU_H
