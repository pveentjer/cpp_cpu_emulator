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
#include <algorithm>
#include <sstream>
#include "instructions.h"
#include "utils.h"

static const int REGISTER_COUNT = 32;
static const int MEMORY_SIZE = 16;
static const int STORE_BUFFER_CAPACITY = 4;
static const int CPU_FREQUENCY_HZ = 3;

using namespace std;

struct StoreBufferEntry
{
    int value;
    int addr;
};

static const int STAGE_FETCH = 1;
static const int STAGE_DECODE = 2;
static const int STAGE_EXECUTE = 3;

struct Slot
{
    Instr *instr;
    int stage;
};

struct StoreBuffer
{
    StoreBufferEntry entries[STORE_BUFFER_CAPACITY];
    uint64_t head;
    uint64_t tail;

    optional<int> lookup(int addr)
    {
        // todo: instead of iterating over all values, there should be a directly-mapped hash-table
        // so that we can use the last 12 bits of the address and do a lookup. Then we also need
        // to handle the 4K aliasing problem.
        for (uint64_t k = tail; k < head; k++)
        {
            StoreBufferEntry &entry = entries[k % STORE_BUFFER_CAPACITY];
            if (entry.addr == addr)
            {
                return optional<int>(entry.value);
            }
        }

        return nullopt;
    }

    void write(int addr, int value)
    {
        StoreBufferEntry &entry = entries[tail % STORE_BUFFER_CAPACITY];
        entry.value = value;
        entry.addr = addr;
        tail++;
    }

    void tick(vector<int> *memory)
    {
        if (head != tail)
        {
            StoreBufferEntry &entry = entries[head % STORE_BUFFER_CAPACITY];
            memory->at(entry.addr) = entry.value;
            head++;
        }
    }
};


using namespace std;

class CPU
{

public:
    int32_t ip = -1;
    vector<Instr> *program;
    vector<int> *registers;
    vector<int> *memory;
    StoreBuffer sb;
    Slot slot;
    // when true, prints every instruction before being executed.
    bool trace;

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
        sb.head = 0;
        sb.tail = 0;
        trace = false;
        slot.stage = STAGE_FETCH;
    }

    void print_memory() const;

    void execute(Instr *instr);

    bool tick_again() const;

    void tick();
};

#endif //CPU_EMULATOR_CPU_H
