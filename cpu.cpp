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
            break;
        }
        case OPCODE_SUB:
        {
            int v1 = registers->at(instr->code.SUB.r_src1);
            int v2 = registers->at(instr->code.SUB.r_src2);
            registers->at(instr->code.SUB.r_dst) = v1 + v2;
            break;
        }
        case OPCODE_AND:
        {
            int v1 = registers->at(instr->code.AND.r_src1);
            int v2 = registers->at(instr->code.AND.r_src2);
            registers->at(instr->code.AND.r_dst) = v1 && v2;
            break;
        }
        case OPCODE_OR:
        {
            int v1 = registers->at(instr->code.OR.r_src1);
            int v2 = registers->at(instr->code.OR.r_src2);
            registers->at(instr->code.OR.r_dst) = v1 || v2;
            break;
        }
        case OPCODE_NOT:
        {
            int v1 = registers->at(instr->code.NOT.r_src);
            registers->at(instr->code.OR.r_dst) = !v1;
            break;
        }
        case OPCODE_CMP:
        {
            int v1 = registers->at(instr->code.CMP.r_src1);
            int v2 = registers->at(instr->code.CMP.r_src2);
            registers->at(instr->code.CMP.r_dst) = v1 == v2;
            break;
        }
        case OPCODE_INC:
        {
            registers->at(instr->code.INC.r_src)++;
            break;
        }
        case OPCODE_DEC:
        {
            registers->at(instr->code.DEC.r_src)--;
            break;
        }
        case OPCODE_MOV:
        {
            registers->at(instr->code.MOV.r_dst) = registers->at(instr->code.MOV.r_src);
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
            break;
        }
        case OPCODE_STORE:
        {
            sb.write(instr->code.STORE.m_dst, registers->at(instr->code.STORE.r_src));
            break;
        }
        case OPCODE_PRINTR:
        {
            int v1 = registers->at(instr->code.PRINTR.r_src);
            printf("                                R%d=%d\n", instr->code.PRINTR.r_src, v1);
            break;
        }
        case OPCODE_JNZ:
        {
            int v1 = registers->at(instr->code.JNZ.r_src);
            if (v1 != 0)
            {
                // the ip will be bumped at the end again, so -1 is subtracted
                ip = instr->code.JNZ.p_target - 1;
            }
            break;
        }
        case OPCODE_HALT:
        {
            // at the end of the cycle, the ip is bumped again and it will
            // end up as -1, and no further instructions will be processed.
            ip = -2;
            break;
        }
        case OPCODE_NOP:
        {
            break;
        }
        default:
            throw runtime_error("Unrecognized opcode");
    }
}

bool CPU::is_idle()
{
    return ip == -1 && sb.is_empty();
}

void CPU::tick()
{
    double pause = 1.0 / cpuFrequencyHz;
    chrono::milliseconds period(static_cast<int>(pause * 1000));
    this_thread::sleep_for(period);

    if (ip > -1)
    {
        bool fetchNext;

        Slot *fetchSlot = &pipeline.slots[pipeline.index % PIPELINE_DEPTH];

        // Fetch
        if (insertNopCount > 0)
        {
            fetchNext = false;
            insertNopCount--;
            pipeline.slots[pipeline.index % PIPELINE_DEPTH].instr = nop;
        }
        else
        {
            fetchNext = true;
            Instr *instr = &program->at(ip);

            // when a branch enters the pipeline, the pipeline will be filled with nops
            // to prevent a control hazard.
            if (instr->opcode == OPCODE_JNZ)
            {
                insertNopCount = PIPELINE_DEPTH - 1;
            }

            fetchSlot->instr = instr;
        }

        // Decode (ignored)
        // Execute
        Slot *executeSlot = &pipeline.slots[(pipeline.index + STAGE_EXECUTE) % PIPELINE_DEPTH];
        if (trace)
        {
            print_instr(executeSlot->instr);
        }
        execute(executeSlot->instr);

        if (fetchNext)
        {
            ip++;
        }
    }

    sb.tick(memory);
    cycles++;
}

void CPU::print_memory() const
{
    printf("------------------Memory----------------\n");
    for (int k = 0; k < memory->size(); k++)
    {
        printf("%04d %04d\n", k, memory->at(k));
    }
}
