//
// Created by pveentjer on 5/3/24.
//

#include "../include/frontend.h"
#include "../include/common.h"


void Frontend::cycle()
{
    // under ideal conditions, the frontend can fetch/decode n instructions.
    for (uint8_t k = 0; k < n_wide; k++)
    {
        if (ip_next_fetch == -1
            || ip_next_fetch >= code->size()
            || instr_queue->is_full()
            || branch_in_pipeline)
        {
            return;
        }

        //printf("ip_next_fetch %d\n", ip_next_fetch);
        Instr *instr = &code->at(ip_next_fetch);

        if (is_branch(instr->opcode))
        {
            branch_in_pipeline = true;
        }
        else
        {
            ip_next_fetch++;
        }
        instr_queue->enqueue(instr);
    }
}

bool Frontend::is_idle()
{
    return ip_next_fetch == -1;
}
