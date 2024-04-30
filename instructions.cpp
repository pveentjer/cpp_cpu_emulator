//
// Created by pveentjer on 4/28/24.
//

#include "instructions.h"


void print_instr(Instr *instr)
{
    switch (instr->opcode)
    {
        case OPCODE_ADD:
            printf("ADD R%d R%d R%d\n", instr->code.ADD.r_dst, instr->code.ADD.r_src1, instr->code.ADD.r_src2);
            break;
        case OPCODE_SUB:
            printf("SUB R%d R%d R%d \n", instr->code.SUB.r_dst, instr->code.SUB.r_src1, instr->code.SUB.r_src2);
            break;
        case OPCODE_AND:
            printf("AND R%d R%d R%d \n", instr->code.AND.r_dst, instr->code.AND.r_src1, instr->code.AND.r_src2);
            break;
        case OPCODE_OR:
            printf("OR R%d R%d R%d \n", instr->code.OR.r_dst, instr->code.OR.r_src1, instr->code.OR.r_src2);
            break;
        case OPCODE_NOT:
            printf("NOT R%d R%d \n", instr->code.NOT.r_dst, instr->code.NOT.r_src);
            break;
        case OPCODE_CMP:
            printf("CMP R%d R%d %d \n", instr->code.CMP.r_dst, instr->code.CMP.r_src1, instr->code.CMP.r_src2);
            break;
        case OPCODE_INC:
            printf("INC R%d \n", instr->code.INC.r_src);
            break;
        case OPCODE_DEC:
            printf("DEC R%d \n", instr->code.DEC.r_src);
            break;
        case OPCODE_MOV:
            printf("MOV R%d R%d\n", instr->code.MOV.r_src, instr->code.MOV.r_dst);
            break;
        case OPCODE_LOAD:
            printf("LOAD R%d %d\n", instr->code.LOAD.r_dst, instr->code.LOAD.m_src);
            break;
        case OPCODE_STORE:
            printf("STORE R%d %d\n", instr->code.STORE.r_src, instr->code.STORE.m_dst);
            break;
        case OPCODE_PRINTR:
            printf("PRINTR R%d\n", instr->code.PRINTR.r_src);
            break;
        case OPCODE_JNZ:
            printf("JNZ R%d code[%d]\n", instr->code.JNZ.r_src, instr->code.JNZ.p_target);
            break;
        case OPCODE_HALT:
            printf("HALT\n");
            break;
        default:
            throw std::runtime_error("Unrecognized opcode");
    }
}