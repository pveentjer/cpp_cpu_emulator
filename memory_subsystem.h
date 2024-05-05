//
// Created by pveentjer on 5/3/24.
//

#ifndef CPU_EMULATOR_MEMORY_SUBSYSTEM_H
#define CPU_EMULATOR_MEMORY_SUBSYSTEM_H

#include <optional>
#include <cstdint>
#include <vector>

using namespace std;

struct StoreBufferEntry
{
    int value;
    int addr;
};

struct StoreBuffer
{
    StoreBufferEntry *entries;
    uint16_t capacity;
    uint64_t head;
    uint64_t tail;
    vector<int> *memory;

    StoreBuffer(uint16_t capacity, vector<int> *memory);

    ~StoreBuffer();

    optional<int> lookup(int addr);

    bool is_empty();

    void write(int addr, int value);

    void cycle();
};


#endif //CPU_EMULATOR_MEMORY_SUBSYSTEM_H
