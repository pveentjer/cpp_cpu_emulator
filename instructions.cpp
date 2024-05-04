//
// Created by pveentjer on 4/28/24.
//

#include "instructions.h"


void print_instr(Instr *instr)
{
    switch (instr->opcode)
    {
        case OPCODE_ADD:
            printf("ADD R%d,R%d,R%d\n", instr->output_ops[0].reg, instr->input_ops[0].reg,
                   instr->input_ops[1].reg);
            break;
        case OPCODE_SUB:
            printf("SUB R%d,R%d,R%d\n", instr->output_ops[0].reg, instr->input_ops[0].reg,
                   instr->input_ops[1].reg);
            break;
        case OPCODE_AND:
            printf("AND R%d,R%d,R%d\n", instr->output_ops[0].reg, instr->input_ops[0].reg,
                   instr->input_ops[1].reg);
            break;
        case OPCODE_OR:
            printf("OR R%d,R%d,R%d\n", instr->output_ops[0].reg, instr->input_ops[0].reg,
                   instr->input_ops[1].reg);
            break;
        case OPCODE_XOR:
            printf("XOR R%d,R%d,R%d\n", instr->output_ops[0].reg, instr->input_ops[0].reg,
                   instr->input_ops[1].reg);
            break;
        case OPCODE_NOT:
            printf("NOT R%d,R%d\n", instr->output_ops[0].reg, instr->input_ops[0].reg);
            break;
//        case OPCODE_CMP:
//            printf("CMP R%d R%d %d \n", instr->code.CMP.r_dst, instr->code.CMP.r_src1, instr->code.CMP.r_src2);
//            break;
        case OPCODE_INC:
            printf("INC R%d\n", instr->output_ops[0].reg);
            break;
        case OPCODE_DEC:
            printf("DEC R%d\n", instr->output_ops[0].reg);
            break;
        case OPCODE_MOV:
            printf("MOV R%d R%d\n", instr->input_ops[0].reg, instr->output_ops[0].reg);
            break;
        case OPCODE_LOAD:
            printf("LOAD R%d,[%lu]\n", instr->output_ops[0].reg, instr->input_ops[0].memory_addr);
            break;
        case OPCODE_STORE:
            printf("STORE R%d,[%lu]\n", instr->input_ops[0].reg, instr->output_ops[0].memory_addr);
            break;
        case OPCODE_PRINTR:
            printf("PRINTR R%d\n", instr->input_ops[0].reg);
            break;
        case OPCODE_JNZ:
            printf("JNZ R%d code[%lu]\n", instr->input_ops[0].reg, instr->input_ops[1].code_addr);
            break;
        case OPCODE_HALT:
            printf("HALT\n");
            break;
        case OPCODE_NOP:
            //printf("NOP\n");
            break;
        default:
            throw std::runtime_error("print_instr:Unrecognized opcode");
    }
}

bool is_branch(Opcode opcode)
{
    switch (opcode)
    {
        case OPCODE_JNZ:
            return true;
        case OPCODE_HALT:
            // for the time being it is a branch instruction to fill the pipeline with nops.
            return true;
        default:
            return false;
    }
}
