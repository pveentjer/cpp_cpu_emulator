//
// Created by pveentjer on 5/3/24.
//

#ifndef CPU_EMULATOR_FRONTEND_H
#define CPU_EMULATOR_FRONTEND_H


#include <vector>
#include "instructions.h"
#include "common.h"

using namespace std;

/**
 * The Frontend is responsible for fetching and decoding instruction
 * and then will place them on the InstrQueue for the backend.
 */
struct Frontend
{
    uint8_t n_wide;
    vector<Instr> *code;
    bool branch_in_pipeline;
    int32_t ip_next_fetch = -1;
    // used for inserting bubbles
    Instr *nop;
    InstrQueue *instr_queue;

    Frontend(CPU_Config *config, InstrQueue *instrQueue)
            : n_wide(config->frontend_n_wide)
            , instr_queue(instrQueue)
    {
        code = new vector<Instr>();
        ip_next_fetch = -1;
        branch_in_pipeline = false;
        nop = new Instr();
        nop->opcode = OPCODE_NOP;
    }

    ~Frontend()
    {
        delete code;
        delete nop;
    }

    void cycle();

    bool is_idle();

};


#endif //CPU_EMULATOR_FRONTEND_H
