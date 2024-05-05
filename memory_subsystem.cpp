//
// Created by pveentjer on 5/3/24.
//


#include "memory_subsystem.h"


optional<int> StoreBuffer::lookup(int addr)
{
    // todo: instead of iterating over all values, there should be a directly-mapped hash-table
    // so that we can use the last 12 bits of the address and do a lookup. Then we also need
    // to handle the 4K aliasing problem.
    for (uint64_t k = tail; k < head; k++)
    {
        StoreBufferEntry &entry = entries[k % capacity];
        if (entry.addr == addr)
        {
            return optional<int>(entry.value);
        }
    }

    return nullopt;
}

bool StoreBuffer::is_empty()
{
    return head == tail;
}

void StoreBuffer::write(int addr, int value)
{
    StoreBufferEntry &entry = entries[tail % capacity];
    entry.value = value;
    entry.addr = addr;
    tail++;
}

void StoreBuffer::cycle()
{
    if (head != tail)
    {
        StoreBufferEntry &entry = entries[head % capacity];
        memory->at(entry.addr) = entry.value;
        head++;
    }
}

StoreBuffer::StoreBuffer(uint16_t capacity, vector<int> *memory) :memory(memory), capacity(capacity)
{

    entries = new StoreBufferEntry[capacity];
    head = 0;
    tail = 0;
}

StoreBuffer::~StoreBuffer()
{
    delete[] entries;
}
