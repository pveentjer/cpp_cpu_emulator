#ifndef CPU_EMULATOR_INSTRUCTIONS_H
#define CPU_EMULATOR_INSTRUCTIONS_H

#include <cstdint>
#include <cstdio>
#include <stdexcept>

// adds 2 registers and writes the result in another
static const int OPCODE_ADD = 1;
// subtracts the second register from the first and writes the result in another
static const int OPCODE_SUB = 2;
// increments the value in a register
static const int OPCODE_INC = 3;
// decrements the value in a register
static const int OPCODE_DEC = 4;

static const int OPCODE_AND = 5;
static const int OPCODE_OR = 6;
static const int OPCODE_NOT = 7;

static const int OPCODE_LOAD = 8;
static const int OPCODE_STORE = 9;
// prints a value from a register
static const int OPCODE_PRINTR = 10;
static const int OPCODE_HALT = 11;
static const int OPCODE_CMP = 12;
static const int OPCODE_JNZ = 13;
// copy between registers
static const int OPCODE_MOV = 14;


struct Instruction {
    int opcode;
    // r_ prefix means it is a register
    // m_ prefix means it is from memory
    // p_ prefix means it is an address in the program
    union {
        struct {
            uint32_t r_src1, r_src2, r_dst;
        } ADD;
        struct {
            uint32_t r_src1, r_src2, r_dst;
        } SUB;
        struct {
            uint32_t r_src1, r_src2, r_dst;
        } AND;
        struct {
            uint32_t r_src1, r_src2, r_dst;
        } OR;
        struct {
            uint32_t r_src, r_dst;
        } NOT;
        struct {
            uint32_t m_src, r_dst;
        } LOAD;
        struct {
            uint32_t r_src, m_dst;
        } STORE;
        struct {
            uint32_t r_src, r_dst;
        } MOV;
        struct {
            uint32_t r_src;
        } PRINTR;
        struct {
            uint32_t r_src;
        } INC;
        struct {
            uint32_t r_src;
        } DEC;
        struct {
        } HALT;
        struct {
            uint32_t r_src1, r_src2, r_dst;
        } CMP;
        struct {
            uint32_t r_src, p_target;
        } JNZ;
    } code;
};

void print_instr(Instruction *instr);

#endif //CPU_EMULATOR_INSTRUCTIONS_H
