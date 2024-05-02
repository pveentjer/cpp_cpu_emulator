#ifndef CPU_EMULATOR_INSTRUCTIONS_H
#define CPU_EMULATOR_INSTRUCTIONS_H

#include <cstdint>
#include <cstdio>
#include <stdexcept>
#include <map>
#include <unordered_map>

enum Opcode {
    OPCODE_ADD,
    OPCODE_SUB,
    OPCODE_INC,
    OPCODE_DEC,
    OPCODE_AND,
    OPCODE_OR,
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


bool is_branch(int opcode);

static const std::unordered_map<std::string, int> MNEMONIC_TO_OPCODE = {
        {"ADD",    OPCODE_ADD},
        {"SUB",    OPCODE_SUB},
        {"AND",    OPCODE_AND},
        {"OR",     OPCODE_OR},
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
    int opcode;
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

#endif //CPU_EMULATOR_INSTRUCTIONS_H
