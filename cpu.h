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

struct StoreBufferEntry
{
    int value;
    int addr;
};

static const int STAGE_FETCH = 0;
static const int STAGE_DECODE = 1;
static const int STAGE_EXECUTE = 0;
static const int PIPELINE_DEPTH = 3;
struct Slot
{
    Instr *instr;
};

struct Pipeline
{
    Slot slots[PIPELINE_DEPTH];
    uint8_t index = 0;
};

struct StoreBuffer
{
    StoreBufferEntry* entries;
    uint16_t capacity;
    uint64_t head = 0;
    uint64_t tail = 0;

    optional<int> lookup(int addr);

    bool is_empty();

    void write(int addr, int value);

    void tick(vector<int> *memory);
};


class CPU;

struct Frontend
{
    CPU *cpu;
    int bubbleSize = 0;

    bool tick();
};

struct Backend
{
    CPU *cpu;
    // when true, prints every instruction before being executed.
    bool trace = false;
    vector<int> *arch_regs;
    StoreBuffer *sb;
    vector<int> *memory;

    void execute(Instr *instr);

    void tick();
};

using namespace std;

struct CPU_Config{
    uint32_t cpu_frequency_Hz = 1;
    // the total available memory in 'ints' the CPU can use.
    uint32_t memory_size = 16;
    // the number of architectural registers
    uint16_t arch_reg_count = 16;
    // true if every instruction execution should be printed
    bool trace = false;
    // the capacity of the store buffer
    uint16_t sb_capacity = 4;
};

class CPU
{

private:

    bool is_idle();

    void tick();

public:
    uint64_t cycles = 0;
    int32_t ip = -1;
    vector<Instr> *code;
    vector<int> *arch_regs;
    vector<int> *memory;
    StoreBuffer sb;
    Pipeline pipeline;
    Instr *nop = new Instr();
    chrono::milliseconds cycle_period_ms;
    Frontend frontend;
    Backend backend;

    CPU(CPU_Config config)
    {
        ip = 0;
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

        nop->opcode = OPCODE_NOP;
        pipeline.slots[STAGE_FETCH].instr = nop;
        pipeline.slots[STAGE_DECODE].instr = nop;
        pipeline.slots[STAGE_EXECUTE].instr = nop;

        double pause = 1.0 / config.cpu_frequency_Hz;
        cycle_period_ms = chrono::milliseconds(static_cast<int>(pause * 1000));

        sb.entries = new StoreBufferEntry[config.sb_capacity];
        sb.capacity = config.sb_capacity;

        frontend.cpu = this;

        backend.cpu = this;
        backend.trace = config.trace;
        backend.arch_regs = arch_regs;
        backend.sb = &sb;
        backend.memory = memory;
    }

    /**
     * Runs the program till completion (including writing the store buffer to memory).
     */
    void run();

    void print_memory() const;

};

#endif //CPU_EMULATOR_CPU_H
