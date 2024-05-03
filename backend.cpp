//
// Created by pveentjer on 5/3/24.
//

#include "backend.h"


void Backend::cycle()
{
    // retire any instruction that has been executed and hasn't been retired yet.
    // instructions can execute out of order, but will retire in order.
    for (uint64_t k = rob.head; k < rob.tail; k++)
    {
        ROB_Slot *slot = &rob.slots[k % rob.capacity];
        if (slot->state == ROB_SLOT_EXECUTED)
        {
            printf("Retiring ");
            print_instr(slot->instr);
            // todo: retire
            rob.head++;
            retire(slot);
        }
        else
        {
            // As soon as we find an instruction that has not been executed, we stop
            break;
        }
    }

    // execute/submit any rs that has all operands ready
    for (uint64_t k = rs_ready_head; k < rs_ready_tail; k++)
    {
        uint16_t rs_index = rs_ready_queue[k % rs_count];

        RS *rs = &rs_array[rs_index];
        if (trace)
        {
            printf("Executing ");
            print_instr(rs->rob_slot->instr);
        }

        eu.rs = rs;
        eu.execute();
        // todo: should become pending once the instruction is queued for an EU
        rs->state = RS_EXECUTED;
    }
    rs_ready_head = rs_ready_tail;

    // Add as many instructions to reservation station
    int unreserved_cnt = rob.tail - rob.reserved;
    for (int k = 0; k < unreserved_cnt; k++)
    {
        if (rs_free_stack_size == 0)
        {

            // There are no free reservation stations, so we are done
            break;
        }

        // get a free RS
        rs_free_stack_size--;
        RS *rs = &rs_array[this->rs_free_stack[rs_free_stack_size]];

        ROB_Slot *slot = &rob.slots[rob.reserved % rob.capacity];
        rob.reserved++;
        init_rs(rs, slot);

        if (rs->state == RS_READY)
        {
            on_rs_ready(rs);
        }
        //print_instr(slot->instr);
    }

    // place instructions from the instruction queue into the rob.
    int cnt = std::min(rob.empty_slots(), instr_queue->size());
    for (int k = 0; k < cnt; k++)
    {
        Instr *instr = instr_queue->dequeue();
        //print_instr(instr);
        uint64_t i = rob.tail % rob.capacity;
        ROB_Slot *slot = &rob.slots[i];
        slot->instr = instr;
        slot->state = ROB_SLOT_NEW;
        rob.tail++;

        //printf("Inserting into ROB ");
        //print_instr(slot->instr);
    }
}

void Backend::init_rs(RS *rs, ROB_Slot *rob_slot)
{
    Instr *instr = rob_slot->instr;

    switch (instr->opcode)
    {
        case OPCODE_ADD:
            rs->src_required_cnt = 2;
            break;
        case OPCODE_SUB:
            rs->src_required_cnt = 2;
            break;
        case OPCODE_AND:
            rs->src_required_cnt = 2;
            break;
        case OPCODE_OR:
            rs->src_required_cnt = 2;
            break;
        case OPCODE_XOR:
            rs->src_required_cnt = 2;
            break;
        case OPCODE_NOT:
            rs->src_required_cnt = 1;
            break;
        case OPCODE_CMP:
            rs->src_required_cnt = 2;
            break;
        case OPCODE_INC:
            rs->src_required_cnt = 1;
            break;
        case OPCODE_DEC:
            rs->src_required_cnt = 1;
            break;
        case OPCODE_MOV:
            rs->src_required_cnt = 1;
            break;
        case OPCODE_LOAD:
            std::runtime_error("not implemented load");
            break;
        case OPCODE_STORE:
            rs->src_required_cnt = 1;
            break;
        case OPCODE_PRINTR:
            rs->src_required_cnt = 1;
            break;
        case OPCODE_JNZ:
            rs->src_required_cnt = 1;
            break;
        case OPCODE_HALT:
            rs->src_required_cnt = 0;
            break;
        case OPCODE_NOP:
            rs->src_required_cnt = 0;
            break;
        default:
            throw runtime_error("Unrecognized opcode");
    }

    rs->rob_slot = rob_slot;
    rs->src_completed_cnt = 0;
    if (rs->src_required_cnt == 0)
    {
        rs->state = RS_READY;
    }
    else
    {
        rs->state = RS_WAITING;
    }
}

bool Backend::is_idle()
{
    return rob.size() == 0;
}

void Backend::on_rs_ready(RS *rs)
{
    uint64_t index = rs_ready_tail % rs_count;
    rs_ready_queue[index] = rs->rs_index;
    rs_ready_tail++;
}

void ExecutionUnit::execute()
{
    // todo: the result needs to be stored in the RS
    ROB_Slot *rob_slot = rs->rob_slot;
    Instr *instr = rob_slot->instr;
    int *src = rs->src;
    switch (instr->opcode)
    {
        case OPCODE_ADD:
        {
            int res = src[0] + src[1];
            //rs_array[rs_target].srcReady(rs_src_index, res);

            break;
        }
        case OPCODE_SUB:
        {
            int res = src[0] - src[1];
            // rs_array[rs_target].srcReady(rs_src_index, res);
            break;
        }
        case OPCODE_AND:
        {
            int res = src[0] && src[1];
            //rs_array[rs_target].srcReady(rs_src_index, res);
            break;
        }
        case OPCODE_OR:
        {
            int res = src[0] || src[1];
            // rs_array[rs_target].srcReady(rs_src_index, res);
            break;
        }
        case OPCODE_XOR:
        {
            int res = src[0] ^ src[1];
            //rs_array[rs_target].srcReady(rs_src_index, res);
            break;
        }
        case OPCODE_NOT:
        {
            int res = !src[0];
            //rs_array[rs_target].srcReady(rs_src_index, res);
            break;
        }
        case OPCODE_CMP:
        {
            int res = src[0] == src[1];
            //rs_array[rs_target].srcReady(rs_src_index, res);
            break;
        }
        case OPCODE_INC:
        {
            int res = src[0] + 1;
            //rs_array[rs_target].srcReady(rs_src_index, res);
            break;
        }
        case OPCODE_DEC:
        {
            int res = src[0] - 1;
            //rs_array[rs_target].srcReady(rs_src_index, res);
            break;
        }
        case OPCODE_MOV:
        {
            arch_regs->at(instr->code.MOV.r_dst) = arch_regs->at(instr->code.MOV.r_src);
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
            break;
        }
        case OPCODE_STORE:
        {
            sb->write(instr->code.STORE.m_dst, arch_regs->at(instr->code.STORE.r_src));
            break;
        }
        case OPCODE_PRINTR:
        {
            printf("                                R%d=%d\n", instr->code.PRINTR.r_src, src[0]);
            break;
        }
        case OPCODE_JNZ:
        {
            int v1 = arch_regs->at(instr->code.JNZ.r_src);
            if (v1 != 0)
            {
                backend->frontend->ip_next_fetch = instr->code.JNZ.p_target;
            }
            break;
        }
        case OPCODE_HALT:
        {
            break;
        }
        case OPCODE_NOP:
        {
            break;
        }
        default:
            throw runtime_error("Unrecognized opcode");
    }

    rob_slot->state = ROB_Slot_State::ROB_SLOT_EXECUTED;
}

//void RS::srcReady(uint16_t src_index, int src)
//{
//    this->src[src_index] = src;
//    src_completed_cnt++;
//
//    if (src_completed_cnt == src_required_cnt)
//    {
//        // all arguments are complete.
//
//        // todo: send it to an available EU
//        ExecutionUnit &eu = backend->eu;
//        // todo: send it to the port for an EU to process
//    }
//}
//
void Backend::retire(ROB_Slot *rob_slot)
{
    Instr *instr = rob_slot->instr;

    switch (instr->opcode)
    {
//        case OPCODE_ADD:
//            backend->arch_regs->at(instr->code.ADD.r_dst) = result;
//            break;
//        case OPCODE_SUB:
//            backend->arch_regs->at(instr->code.SUB.r_dst) = result;
//            break;
//        case OPCODE_AND:
//            backend->arch_regs->at(instr->code.AND.r_dst) = result;
//            break;
//        case OPCODE_OR:
//            backend->arch_regs->at(instr->code.OR.r_dst) = result;
//            break;
//        case OPCODE_NOT:
//            backend->arch_regs->at(instr->code.NOT.r_dst) = result;
//            break;
//        case OPCODE_CMP:
//            backend->arch_regs->at(instr->code.CMP.r_dst) = result;
//            break;
//        case OPCODE_INC:
//            backend->arch_regs->at(instr->code.INC.r_src) = result;
//            break;
//        case OPCODE_DEC:
//            backend->arch_regs->at(instr->code.DEC.r_src) = result;
//            break;
//        case OPCODE_MOV:
//            backend->arch_regs->at(instr->code.MOV.r_dst) = result;
//            break;
//        case OPCODE_LOAD:
//            backend->arch_regs->at(instr->code.LOAD.r_dst) = result;
//            break;
//        case OPCODE_STORE:
//            backend->sb->write(instr->code.STORE.m_dst, result);
//            break;
//        case OPCODE_PRINTR:
//        {
//            int v1 = backend->arch_regs->at(instr->code.PRINTR.r_src);
//            printf("                                R%d=%d\n", instr->code.PRINTR.r_src, v1);
//            break;
//        }
//        case OPCODE_JNZ:
//        {
////            int v1 = arch_regs->at(instr->code.JNZ.r_src);
////            if (v1 != 0)
////            {
////                cpu->frontend.ip_next_fetch = instr->code.JNZ.p_target;
////            }
//            break;
//        }
        case OPCODE_HALT:
            frontend->ip_next_fetch = -1;
            break;
        case OPCODE_NOP:
            break;
        default:
            throw runtime_error("Unrecognized opcode");
    }
//
//
//    // todo: only when the result is written, the RS is freed.
//    // the rs can be returned to the pool
//    backend->rs_free_stack[backend->rs_free_stack_size] = rs_index;
//    backend->rs_free_stack_size++;
}
