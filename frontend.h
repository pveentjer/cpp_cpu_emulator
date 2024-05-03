//
// Created by pveentjer on 5/3/24.
//

#ifndef CPU_EMULATOR_FRONTEND_H
#define CPU_EMULATOR_FRONTEND_H


#include <vector>
#include "instructions.h"

using namespace std;

/**
 * The Frontend is responsible for fetching and decoding instruction
 * and then will place them on the InstrQueue for the backend.
 */
struct Frontend
{
    vector<Instr> *code;
    uint8_t bubble_remain;
    int32_t ip_next_fetch = -1;
    // used for inserting bubbles
    Instr *nop;
    InstrQueue *instr_queue;

    void cycle();

    bool is_idle();

};


#endif //CPU_EMULATOR_FRONTEND_H
