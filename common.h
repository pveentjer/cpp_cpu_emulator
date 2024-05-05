//
// Created by pveentjer on 5/3/24.
//

#ifndef CPU_EMULATOR_COMMON_H
#define CPU_EMULATOR_COMMON_H

#include <cstdint>

// currently just 2 stages; fetch + decode
static const int PIPELINE_DEPTH = 10;
struct CPU_Config
{
    uint32_t cpu_frequency_Hz = 1;
    // the total available memory_addr in 'ints' the CPU can use.
    uint32_t memory_size_ints = 16;
    // the number of architectural registers
    uint16_t arch_reg_cnt = 8;
    // the number of physical registers
    uint16_t phys_reg_cnt = 64;
    // true if every instruction execution should be printed
    bool trace = false;
    // the capacity of the store buffer
    uint16_t sb_capacity = 4;
    // the capacity of the instruction queue between frontend and backend
    uint8_t instr_queue_capacity = 16;
    // the size of the reorder buffer
    uint8_t rob_capacity = 16;
    // the number of reservation stations
    uint16_t rs_count = 16;
    // the number of instructions that can be fetched/decoded in a single cycle.
    uint8_t frontend_n_wide = 8;
};
#endif //CPU_EMULATOR_COMMON_H
