#ifndef CPU_EMULATOR_INSTRUCTIONS_H
#define CPU_EMULATOR_INSTRUCTIONS_H

#include <cstdint>
#include <cstdio>
#include <stdexcept>
#include <map>
#include <unordered_map>

enum Opcode
{
    OPCODE_ADD,
    OPCODE_SUB,
    OPCODE_INC,
    OPCODE_DEC,
    OPCODE_AND,
    OPCODE_OR,
    OPCODE_XOR,
    OPCODE_NOT,
    OPCODE_LOAD,
    OPCODE_STORE,
    OPCODE_PRINTR,
    OPCODE_HALT,
    OPCODE_CMP,
    OPCODE_JNZ,
    OPCODE_MOV,
    OPCODE_NOP
};


bool is_branch(Opcode opcode);

static const std::unordered_map<std::string, Opcode> MNEMONIC_TO_OPCODE = {
        {"ADD",    OPCODE_ADD},
        {"SUB",    OPCODE_SUB},
        {"AND",    OPCODE_AND},
        {"OR",     OPCODE_OR},
        {"XOR",    OPCODE_XOR},
        {"NOT",    OPCODE_NOT},
        {"CMP",    OPCODE_CMP},
        {"MOV",    OPCODE_MOV},
        {"LOAD",   OPCODE_LOAD},
        {"STORE",  OPCODE_STORE},
        {"PRINTR", OPCODE_PRINTR},
        {"INC",    OPCODE_INC},
        {"DEC",    OPCODE_DEC},
        {"JNZ",    OPCODE_JNZ},
        {"HALT",   OPCODE_HALT},
        {"NOP",    OPCODE_NOP}
};

struct Instr
{
    Opcode opcode;
    // r_ prefix means it is a register
    // m_ prefix means it is from memory
    // p_ prefix means it is an address in the program
    union
    {
        struct
        {
            uint32_t r_src1, r_src2, r_dst;
        } ADD;
        struct
        {
            uint32_t r_src1, r_src2, r_dst;
        } SUB;
        struct
        {
            uint32_t r_src1, r_src2, r_dst;
        } AND;
        struct
        {
            uint32_t r_src1, r_src2, r_dst;
        } OR;
        struct
        {
            uint32_t r_src1, r_src2, r_dst;
        } XOR;
        struct
        {
            uint32_t r_src, r_dst;
        } NOT;
        struct
        {
            uint32_t m_src, r_dst;
        } LOAD;
        struct
        {
            uint32_t r_src, m_dst;
        } STORE;
        struct
        {
            uint32_t r_src, r_dst;
        } MOV;
        struct
        {
            uint32_t r_src;
        } PRINTR;
        struct
        {
            uint32_t r_src;
        } INC;
        struct
        {
            uint32_t r_src;
        } DEC;
        struct
        {
        } HALT;
        struct
        {
            uint32_t r_src1, r_src2, r_dst;
        } CMP;
        struct
        {
            uint32_t r_src, p_target;
        } JNZ;
    } code;
};

void print_instr(Instr *instr);


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


#endif //CPU_EMULATOR_INSTRUCTIONS_H
