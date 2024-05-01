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
#include <fstream>
//#include <algorithm>
#include <sstream>
#include "instructions.h"
#include "utils.h"

static const int REGISTER_COUNT = 32;
static const int MEMORY_SIZE = 16;
static const int STORE_BUFFER_CAPACITY = 4;


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
    StoreBufferEntry entries[STORE_BUFFER_CAPACITY];
    uint64_t head = 0;
    uint64_t tail = 0;

    optional<int> lookup(int addr);

    bool is_empty();

    void write(int addr, int value);

    void tick(vector<int> *memory);
};


using namespace std;

class CPU
{

private:
    void execute(Instr *instr);

    bool is_idle();

    void tick();

public:
    uint64_t cycles = 0;
    int32_t ip = -1;
    vector<Instr> *program;
    vector<int> *registers;
    vector<int> *memory;
    StoreBuffer sb;
    // when true, prints every instruction before being executed.
    bool trace;
    Pipeline pipeline;
    int bubbleSize = 0;
    Instr *nop = new Instr();
    chrono::milliseconds cycle_period_ms;

    CPU()
    {
        ip = 0;
        program = new vector<Instr>();
        registers = new vector<int>();
        for (int k = 0; k < REGISTER_COUNT; k++)
        {
            registers->push_back(0);
        }
        memory = new vector<int>();
        for (int k = 0; k < MEMORY_SIZE; k++)
        {
            memory->push_back(0);
        }

        trace = false;
        nop->opcode = OPCODE_NOP;
        pipeline.slots[STAGE_FETCH].instr = nop;
        pipeline.slots[STAGE_DECODE].instr = nop;
        pipeline.slots[STAGE_EXECUTE].instr = nop;
        cycle_period_ms = chrono::milliseconds(static_cast<int>(1000));
    }

    /**
     * Runs the program till completion (including writing the store buffer to memory).
     */
    void run();

    void setTrace(bool trace);

    void setCpuFrequencyHz(uint32_t cpuFrequencyHz);

    void print_memory() const;

    void tick_backend();

    bool tick_frontend();
};

#endif //CPU_EMULATOR_CPU_H
