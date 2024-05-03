//
// Created by pveentjer on 5/3/24.
//

#include "backend.h"



void Backend::cycle()
{
    // execute 1 instruction for the time (in order)
    if (!rob.is_empty())
    {
        ROB_Slot *slot = &rob.slots[rob.head % rob.capacity];
        if (trace)
        {
            print_instr(slot->instr);
        }

       // eu.slot = slot;
        eu.execute();
        rob.head++;
    }

    // place instructions from the instruction queue into the rob.
    int cnt = std::min(rob.empty_slots(), instr_queue->size());
    for (int k = 0; k < cnt; k++)
    {
        Instr *instr = instr_queue->dequeue();
        ROB_Slot *slot = &rob.slots[rob.tail % rob.capacity];
        slot->instr = instr;
        slot->state = SLOT_NEW;
        rob.tail++;
    }

    // any instruction that hasn't been placed in a RS, should be placed in any available RS.

}

bool Backend::is_idle()
{
    return rob.size() == 0;
}

void ExecutionUnit::execute()
{
    // todo: the result needs to be stored in the RS
    ROB_Slot *slot = rs->robSlot;
    Instr *instr = slot->instr;
    int *src = rs->src;
    switch (instr->opcode)
    {
        case OPCODE_ADD:
        {
            int res = src[0] + src[1];
            rs_array[rs_target].srcReady(rs_src_index, res);

            slot->state = SLOT_EXECUTED;
            break;
        }
        case OPCODE_SUB:
        {
            int res = src[0] - src[1];
            rs_array[rs_target].srcReady(rs_src_index, res);
            slot->state = SLOT_EXECUTED;
            break;
        }
        case OPCODE_AND:
        {
            int res = src[0] && src[1];
            rs_array[rs_target].srcReady(rs_src_index, res);
            slot->state = SLOT_EXECUTED;
            break;
        }
        case OPCODE_OR:
        {
            int res = src[0] || src[1];
            rs_array[rs_target].srcReady(rs_src_index, res);
            slot->state = SLOT_EXECUTED;
            break;
        }
        case OPCODE_XOR:
        {
            int res = src[0] ^ src[1];
            rs_array[rs_target].srcReady(rs_src_index, res);
            slot->state = SLOT_EXECUTED;
            break;
        }
        case OPCODE_NOT:
        {
            int res = !src[0];
            rs_array[rs_target].srcReady(rs_src_index, res);
            slot->state = SLOT_EXECUTED;
            break;
        }
        case OPCODE_CMP:
        {
            int res = src[0] == src[1];
            rs_array[rs_target].srcReady(rs_src_index, res);
            slot->state = SLOT_EXECUTED;
            break;
        }
        case OPCODE_INC:
        {
            int res = src[0] + 1;
            rs_array[rs_target].srcReady(rs_src_index, res);
            slot->state = SLOT_EXECUTED;
            break;
        }
        case OPCODE_DEC:
        {
            int res = src[0] - 1;
            rs_array[rs_target].srcReady(rs_src_index, res);
            slot->state = SLOT_EXECUTED;
            break;
        }
        case OPCODE_MOV:
        {
            arch_regs->at(instr->code.MOV.r_dst) = arch_regs->at(instr->code.MOV.r_src);
            slot->state = SLOT_EXECUTED;
            break;
        }
        case OPCODE_LOAD:
        {
            // a primitive version of store to load forwarding. Because of the store buffer
            // we first need to look there before returning the value otherwise the CPU would
            // not be able to see some of its own writes and become incoherent.

            int value = sb->lookup(instr->code.LOAD.m_src)
                    .value_or(memory->at(instr->code.LOAD.m_src));

            arch_regs->at(instr->code.LOAD.r_dst) = value;
            slot->state = SLOT_EXECUTED;
            break;
        }
        case OPCODE_STORE:
        {
            sb->write(instr->code.STORE.m_dst, arch_regs->at(instr->code.STORE.r_src));
            slot->state = SLOT_EXECUTED;
            break;
        }
        case OPCODE_PRINTR:
        {
            printf("                                R%d=%d\n", instr->code.PRINTR.r_src, src[0]);
            slot->state = SLOT_EXECUTED;
            break;
        }
        case OPCODE_JNZ:
        {
            int v1 = arch_regs->at(instr->code.JNZ.r_src);
            if (v1 != 0)
            {
                backend->frontend->ip_next_fetch = instr->code.JNZ.p_target;
            }
            slot->state = SLOT_EXECUTED;
            break;
        }
        case OPCODE_HALT:
        {
            backend->frontend->ip_next_fetch = -1;
            slot->state = SLOT_EXECUTED;
            break;
        }
        case OPCODE_NOP:
        {
            slot->state = SLOT_EXECUTED;
            break;
        }
        default:
            throw runtime_error("Unrecognized opcode");
    }
}

void RS::srcReady(uint16_t src_index, int src)
{
    this->src[src_index] = src;
    src_completed_cnt++;

    if (src_completed_cnt == src_required_cnt)
    {
        // all arguments are complete.

        // todo: send it to an available EU
        ExecutionUnit &eu = backend->eu;
        // todo: send it to the port for an EU to process
    }
}

void RS::storeResult()
{
    Instr *instr = robSlot->instr;

    switch (instr->opcode)
    {
        case OPCODE_ADD:
            backend->arch_regs->at(instr->code.ADD.r_dst) = result;
            break;
        case OPCODE_SUB:
            backend->arch_regs->at(instr->code.SUB.r_dst) = result;
            break;
        case OPCODE_AND:
            backend->arch_regs->at(instr->code.AND.r_dst) = result;
            break;
        case OPCODE_OR:
            backend->arch_regs->at(instr->code.OR.r_dst) = result;
            break;
        case OPCODE_NOT:
            backend->arch_regs->at(instr->code.NOT.r_dst) = result;
            break;
        case OPCODE_CMP:
            backend->arch_regs->at(instr->code.CMP.r_dst) = result;
            break;
        case OPCODE_INC:
            backend->arch_regs->at(instr->code.INC.r_src) = result;
            break;
        case OPCODE_DEC:
            backend->arch_regs->at(instr->code.DEC.r_src) = result;
            break;
        case OPCODE_MOV:
            backend->arch_regs->at(instr->code.MOV.r_dst) = result;
            break;
        case OPCODE_LOAD:
            backend->arch_regs->at(instr->code.LOAD.r_dst) = result;
            break;
        case OPCODE_STORE:
            backend->sb->write(instr->code.STORE.m_dst, result);
            break;
        case OPCODE_PRINTR:
        {
            int v1 = backend->arch_regs->at(instr->code.PRINTR.r_src);
            printf("                                R%d=%d\n", instr->code.PRINTR.r_src, v1);
            break;
        }
        case OPCODE_JNZ:
        {
//            int v1 = arch_regs->at(instr->code.JNZ.r_src);
//            if (v1 != 0)
//            {
//                cpu->frontend.ip_next_fetch = instr->code.JNZ.p_target;
//            }
            break;
        }
        case OPCODE_HALT:
            backend->frontend->ip_next_fetch = -1;
            break;
        case OPCODE_NOP:
            break;
        default:
            throw runtime_error("Unrecognized opcode");
    }


    // todo: only when the result is written, the RS is freed.
    // the rs can be returned to the pool
    backend->rs_free_array[backend->rs_free_count] = rs_index;
    backend->rs_free_count++;
}
