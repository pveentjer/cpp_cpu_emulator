//
// Created by pveentjer on 5/3/24.
//

#include "backend.h"
#include "utils.h"

void Backend::cycle()
{
    cycle_retire();

    // send for execution units
    cycle_dispatch();

    // select reservation stations
    cycle_issue();

    // place instructions from the instruction queue into the rob.
    int cnt = std::min(rob.empty_slots(), instr_queue->size());
    for (int k = 0; k < cnt; k++)
    {
        Instr *instr = instr_queue->dequeue();
        //print_instr(instr);
        uint64_t i = rob.tail % rob.capacity;
        ROB_Slot *slot = &rob.slots[i];
        slot->instr = instr;
        slot->state = ROB_SLOT_FREE;
        rob.tail++;

        //printf("Inserting into ROB ");
        //print_instr(slot->instr);
    }
}

// Add as many instructions to RS's
void Backend::cycle_issue()
{
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
        RS *rs = &rs_array[rs_free_stack[rs_free_stack_size]];

        ROB_Slot *rob_slot = &rob.slots[rob.reserved % rob.capacity];
        rob_slot->rs = rs;

        rob.reserved++;
        Instr *instr = rob_slot->instr;
        rs->rob_slot = rob_slot;

        // prepare the input operands.
        rs->input_op_cnt = instr->input_ops_cnt;
        rs->input_opt_ready_cnt = 0;
        for (int l = 0; l < instr->input_ops_cnt; l++)
        {
            Operand *input_op_instr = &instr->input_ops[l];
            Operand *input_op_rs = &rs->input_ops[l];
            switch (input_op_instr->type)
            {
                case OperandType::REGISTER:
                {
                    // The rat_entry will determine if there is a physical register we should use or
                    // if the architectural register should be used.
                    uint16_t arch_reg = input_op_instr->reg;
                    RAT_Entry *rat_entry = &rat->entries[arch_reg];

                    if (rat_entry->valid)
                    {
                        // we need to use the physical register for the value
                        Phys_Reg *phys_reg = &phys_reg_array[rat_entry->phys_reg];
                        if (phys_reg->valid)
                        {
                            // the physical register has the value, so use that
                            input_op_rs->type = OperandType::CONSTANT;
                            input_op_rs->constant = phys_reg->value;
                            rs->input_opt_ready_cnt++;
                        }
                        else
                        {
                            // the physical register doesn't have the value.
                            // the broadcast on the cdb will take care of setting the value
                            input_op_rs->reg = rat_entry->phys_reg;
                            input_op_rs->type = REGISTER;
                        }
                    }
                    else
                    {
                        // there is no physical register, so we use the value in the architectural register
                        input_op_rs->type = OperandType::CONSTANT;
                        input_op_rs->constant = arch_regs[arch_reg];
                        rs->input_opt_ready_cnt++;
                    }

                    break;
                }
                case OperandType::CODE:
                    input_op_rs->type = CONSTANT;
                    input_op_rs->code_addr = input_op_instr->code_addr;
                    rs->input_opt_ready_cnt++;
                    break;
                case OperandType::CONSTANT:
                    input_op_rs->type = CONSTANT;
                    input_op_rs->constant = input_op_instr->constant;
                    rs->input_opt_ready_cnt++;
                    break;
                case OperandType::MEMORY:
                    input_op_rs->type = MEMORY;
                    input_op_rs->memory_addr = input_op_instr->memory_addr;
                    rs->input_opt_ready_cnt++;
                    break;
                default:
                    throw std::runtime_error("Unhandled operand type while renaming");
            }
        }

        // prepare the output operands
        for (int l = 0; l < instr->output_ops_cnt; l++)
        {
            Operand *output_op_instr = &instr->output_ops[l];
            Operand *output_op_rs = &rs->output_ops[l];
            output_op_rs->type = output_op_instr->type;
            switch (output_op_instr->type)
            {
                case OperandType::REGISTER:
                {
                    if(phys_reg_free_stack_size == 0){
                        throw std::runtime_error("there are no more physical registers");
                    }

                    // get a free physical register.
                    phys_reg_free_stack_size--;
                    uint16_t phys_reg = phys_reg_free_stack[phys_reg_free_stack_size];

                    rat->entries[output_op_instr->reg].phys_reg = phys_reg;
                    output_op_rs->reg = phys_reg;
                    printf("Register rename from %d to %d\n", output_op_instr->reg, output_op_rs->reg);
                    break;
                }
//                case OperandType::MEMORY:
//                    output_op_rs->memory_addr = output_op_instr->memory_addr;
//                    rs->input_opt_ready_cnt++;
//                    break;
                default:
                    throw std::runtime_error("Unhandled operand type while renaming");
            }
        }


        if (rs->input_op_cnt == rs->input_opt_ready_cnt)
        {
            printf("Issue READY ");
            print_instr(rs->rob_slot->instr);


            rs->state = RS_READY;
            on_rs_ready(rs);
        }
        else
        {
            printf("Issue ISSUED ");
            print_instr(rs->rob_slot->instr);
            rs->state = RS_ISSUED;
        }

        //print_instr(rob_slot->instr);
    }
}

// The dispatch: so sending ready reservation stations to execution units.
void Backend::cycle_dispatch()
{// issue any rs that has all in_operands ready
    for (uint64_t k = rs_ready_head; k < rs_ready_tail; k++)
    {
        uint16_t rs_index = rs_ready_queue[k % rs_count];

        RS *rs = &rs_array[rs_index];
        if (trace)
        {
            printf("Dispatch (execute) ");
            print_instr(rs->rob_slot->instr);
        }

        eu.rs = rs;

        // prepare the in_operands
        Instr *instr = rs->rob_slot->instr;
        for (int op_index = 0; op_index < instr->input_ops_cnt; op_index++)
        {
            Operand *operand = &rs->input_ops[op_index];
            eu.in_operands[op_index].type = operand->type;

            switch (operand->type)
            {
                case CONSTANT:
                    eu.in_operands[op_index].constant = operand->constant;
                    break;
                case REGISTER:
                    // we loop up the physical register for the architectural register
                    // and then load the value
                    //eu.in_operands[op_index].constant =
                    break;
                case MEMORY:
                    eu.in_operands[op_index].memory_addr = operand->memory_addr;
                    break;
                case CODE:
                    eu.in_operands[op_index].code_addr = operand->code_addr;
                    break;
                default:
                    throw runtime_error("Backend::cycle: Unknown operand type");
            }
        }

        eu.execute();

        int result = eu.result;

        rs->rob_slot->result = result;
        rs->rob_slot->state = ROB_SLOT_EXECUTED;

        // todo: should become pending once the instruction is queued for an EU
        rs->state = RS_COMPLETED;

        for (int out_op_index = 0; out_op_index < instr->output_ops_cnt; out_op_index++)
        {
            Operand *out_op = &rs->output_ops[out_op_index];
            if (out_op->type == OperandType::REGISTER)
            {
                // update the physical register.
                Phys_Reg *phys_reg = &phys_reg_array[out_op->reg];
                phys_reg->valid = true;
                phys_reg->value = result;

                // Broadcast the value to any RS that needs it.
                cdb_broadcast(out_op->reg, result);
            }
        }

        // should the phys register be invalidated here?
    }
    rs_ready_head = rs_ready_tail;
}

void Backend::cdb_broadcast(uint16_t phys_reg, int result)
{// broadcast the value.
// Iterate over all RS that are in RS_ISSUED (so waiting)

    for (int k = 0; k < rs_count; k++)
    {
        RS *rs = &rs_array[k];

        if (rs->state != RS_ISSUED)
        {
            continue;
        }

        // iterate over all input operands of the rs
        for (int l = 0; l < rs->rob_slot->instr->input_ops_cnt; l++)
        {

            Operand *target_rs_in_op = &rs->input_ops[l];
            if (target_rs_in_op->type != REGISTER || target_rs_in_op->reg != phys_reg)
            {
                continue;
            }

            // Directly update the value
            target_rs_in_op->type = CONSTANT;
            target_rs_in_op->constant = result;
            rs->input_opt_ready_cnt++;

            if (rs->input_op_cnt == rs->input_opt_ready_cnt)
            {
                rs->state = RS_READY;
                on_rs_ready(rs);
            }
        }
    }
}

void Backend::cycle_retire()
{// retire any instruction that has been executed and hasn't been retired yet.
// instructions can execute out of order, but will retire in order.
    for (uint64_t k = rob.head; k < rob.tail; k++)
    {
        ROB_Slot *slot = &rob.slots[k % rob.capacity];
        if (slot->state == ROB_SLOT_EXECUTED)
        {
            printf("Retiring ");
            print_instr(slot->instr);


            // todo: retire
            retire(slot);
            rob.head++;
        }
        else
        {
            // As soon as we find an instruction that has not been executed, we stop
            break;
        }
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
    switch (instr->opcode)
    {
        case OPCODE_ADD:
            result = in_operands[0].constant + in_operands[1].constant;
            break;
        case OPCODE_SUB:
            result = in_operands[0].constant - in_operands[1].constant;
            break;
        case OPCODE_AND:
            result = in_operands[0].constant & in_operands[1].constant;
            break;
        case OPCODE_OR:
            result = in_operands[0].constant | in_operands[1].constant;
            break;
        case OPCODE_XOR:
            result = in_operands[0].constant ^ in_operands[1].constant;
            break;
        case OPCODE_NOT:
            result = !in_operands[0].constant;
            break;
//        case OPCODE_CMP:
//        {
//            int res = src[0] == src[1];
//            //rs_array[rs_target].srcReady(rs_src_index, res);
//            break;
//        }
        case OPCODE_INC:
            result = in_operands[0].constant + 1;
            break;
        case OPCODE_DEC:
            result = in_operands[0].constant - 1;
            break;

//        case OPCODE_MOV:
//        {
//            arch_regs->at(instr->code.MOV.r_dst) = arch_regs->at(instr->code.MOV.r_src);
//            break;
//        }
        case OPCODE_LOAD:
        {
            // a primitive version of store to load forwarding. Because of the store buffer
            // we first need to look there before returning the value otherwise the CPU would
            // not be able to see some of its own writes and become incoherent.

            // todo: Load to store forwarding
            result = backend->memory->at(in_operands[0].memory_addr);
//                    sb->lookup(instr->code.LOAD.m_src)
//                    .value_or(memory_addr->at(instr->code.LOAD.m_src));

            //arch_regs->at(instr->code.LOAD.r_dst) = value;
            break;
        }
//        case OPCODE_STORE:
//        {
//            sb->write(instr->code.STORE.m_dst, arch_regs->at(instr->code.STORE.r_src));
//            break;
//        }
        case OPCODE_PRINTR:
        {
            Operand &operand = in_operands[0];
            if (operand.type != CONSTANT)
            {
                printf("wtf\n");
            }
            int i = operand.constant;
            printf("                                R%d=%d\n", instr->input_ops[0].reg, i);
            break;
        }
//        case OPCODE_JNZ:
//        {
//            int v1 = arch_regs->at(instr->code.JNZ.r_src);
//            if (v1 != 0)
//            {
//                backend->frontend->ip_next_fetch = instr->code.JNZ.c_target;
//            }
//            break;
//        }
        case OPCODE_HALT:
            break;
        case OPCODE_NOP:
            break;
        default:
            throw runtime_error("Execute:Unrecognized opcode");
    }


    rob_slot->state = ROB_Slot_State::ROB_SLOT_EXECUTED;
}

void Backend::retire(ROB_Slot *rob_slot)
{
    Instr *instr = rob_slot->instr;

    for (int out_op_index = 0; out_op_index < instr->output_ops_cnt; out_op_index++)
    {
        Operand *out_op = &rob_slot->rs->output_ops[out_op_index];
        if (out_op->type == OperandType::REGISTER)
        {
            // update the architectural register
            arch_regs[instr->output_ops[out_op_index].reg] = rob_slot->result;

            // invalidate the physical register
            Phys_Reg &phys_reg = phys_reg_array[out_op->reg];
            phys_reg.valid = false;

            // return the physical register to the free stack
            phys_reg_free_stack[phys_reg_free_stack_size] = phys_reg.id;
            phys_reg_free_stack_size++;
        }
    }


    // hack to deal with updating the front-end
    if (instr->opcode == OPCODE_HALT)
    {
        frontend->ip_next_fetch = -1;
    }

//        case OPCODE_STORE:
//            // write the result to memory
//            sb->write(instr->output_ops[0].memory_addr, rob_slot->result);
//            break;
//        case OPCODE_PRINTR:
//            break;

//        case OPCODE_JNZ:
//        {
////            int v1 = arch_regs->at(instr->code.JNZ.r_src);
////            if (v1 != 0)
////            {
////                cpu->frontend.ip_next_fetch = instr->code.JNZ.c_target;
////            }
//            break;
//        }

    RS *rs = rob_slot->rs;
    rs->state = RS_FREE;
    rs_free_stack[rs_free_stack_size] = rs->rs_index;
    rs_free_stack_size++;
    rob_slot->rs = nullptr;
}

