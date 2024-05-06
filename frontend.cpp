//
// Created by pveentjer on 5/3/24.
//

#include "frontend.h"
#include "common.h"


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

        // when a branch enters the pipeline, the pipeline will be filled with nops
        // to prevent a branch_in_pipeline hazard. This will guarantee that the branch instruction
        // has been executed, before instructions of the taken or untaken branch are
        // added to the pipeline.
        if (is_branch(instr->opcode))
        {
            branch_in_pipeline = true;
        }
        instr_queue->enqueue(instr);
        ip_next_fetch++;
    }
}

bool Frontend::is_idle()
{
    return ip_next_fetch == -1;
}
