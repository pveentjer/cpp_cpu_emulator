//
// Created by pveentjer on 4/30/24.
//

#include "cpu.h"

void CPU::execute(Instr *instr)
{
    switch (instr->opcode)
    {
        case OPCODE_ADD:
        {
            int v1 = registers->at(instr->code.ADD.r_src1);
            int v2 = registers->at(instr->code.ADD.r_src2);
            registers->at(instr->code.ADD.r_dst) = v1 + v2;
            ip++;
            break;
        }
        case OPCODE_SUB:
        {
            int v1 = registers->at(instr->code.SUB.r_src1);
            int v2 = registers->at(instr->code.SUB.r_src2);
            registers->at(instr->code.SUB.r_dst) = v1 + v2;
            ip++;
            break;
        }
        case OPCODE_AND:
        {
            int v1 = registers->at(instr->code.AND.r_src1);
            int v2 = registers->at(instr->code.AND.r_src2);
            registers->at(instr->code.AND.r_dst) = v1 && v2;
            ip++;
            break;
        }
        case OPCODE_OR:
        {
            int v1 = registers->at(instr->code.OR.r_src1);
            int v2 = registers->at(instr->code.OR.r_src2);
            registers->at(instr->code.OR.r_dst) = v1 || v2;
            ip++;
            break;
        }
        case OPCODE_NOT:
        {
            int v1 = registers->at(instr->code.NOT.r_src);
            registers->at(instr->code.OR.r_dst) = !v1;
            ip++;
            break;
        }
        case OPCODE_CMP:
        {
            int v1 = registers->at(instr->code.CMP.r_src1);
            int v2 = registers->at(instr->code.CMP.r_src2);
            registers->at(instr->code.CMP.r_dst) = v1 == v2;
            ip++;
            break;
        }
        case OPCODE_INC:
        {
            registers->at(instr->code.INC.r_src)++;
            ip++;
            break;
        }
        case OPCODE_DEC:
        {
            registers->at(instr->code.DEC.r_src)--;
            ip++;
            break;
        }
        case OPCODE_MOV:
        {
            registers->at(instr->code.MOV.r_dst) = registers->at(instr->code.MOV.r_src);
            ip++;
            break;
        }
        case OPCODE_LOAD:
        {
            // a primitive version of store to load forwarding. Because of the store buffer
            // we first need to look there before returning the value otherwise the CPU would
            // not be able to see some of its own writes and become incoherent.

            int value = sb.lookup(instr->code.LOAD.m_src)
                    .value_or(memory->at(instr->code.LOAD.m_src));

            registers->at(instr->code.LOAD.r_dst) = value;
            ip++;
            break;
        }
        case OPCODE_STORE:
        {
            sb.write(instr->code.STORE.m_dst, registers->at(instr->code.STORE.r_src));
            ip++;
            break;
        }
        case OPCODE_PRINTR:
        {
            int v1 = registers->at(instr->code.PRINTR.r_src);
            printf("R%d=%d\n", instr->code.PRINTR.r_src, v1);
            ip++;
            break;
        }
        case OPCODE_JNZ:
        {
            int v1 = registers->at(instr->code.JNZ.r_src);
            if (v1 != 0)
            {
                ip = instr->code.JNZ.p_target;
            }
            else
            {
                ip++;
            }
            break;
        }
        case OPCODE_HALT:
        {
            ip = -1;
            break;
        }
        default:
            throw runtime_error("Unrecognized opcode");
    }
}

bool CPU::tick_again() const
{
    if (ip > -1)
    {
        return false;
    }
    return sb.head == sb.tail;
}

void CPU::tick()
{
    double pause = 1.0 / CPU_FREQUENCY_HZ;
    chrono::milliseconds period(static_cast<int>(pause * 1000));
    this_thread::sleep_for(period);

    if (ip > -1)
    {
        if (instr_state.phase == 0)
        {
            instr_state.phase = 1;
            instr_state.instr = &program->at(ip);
        }

        if (instr_state.phase == 1 && trace)
        {
            print_instr(instr_state.instr);
        }

        if (instr_state.phase == instr_latency)
        {
            execute(instr_state.instr);
            instr_state.phase = 0;
            instr_state.instr = nullptr;
        }
        else
        {
            instr_state.phase++;
        }
    }

    sb.tick(memory);
}

void CPU::print_memory() const
{
    printf("------------------Memory----------------\n");
    for (int k = 0; k < memory->size(); k++)
    {
        printf("%04d %04d\n", k, memory->at(k));
    }
}
