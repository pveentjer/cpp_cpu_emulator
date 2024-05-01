//
// Created by pveentjer on 4/30/24.
//

#include "cpu.h"


bool CPU::is_idle()
{
    return ip == -1 && sb.is_empty();
}

void CPU::tick()
{
    if (ip > -1)
    {
        bool fetchNext = frontend.tick();

        backend.tick();

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

void CPU::run()
{
    while (!is_idle())
    {
        this_thread::sleep_for(cycle_period_ms);
        tick();
    }
}

optional<int> StoreBuffer::lookup(int addr)
{
    // todo: instead of iterating over all values, there should be a directly-mapped hash-table
    // so that we can use the last 12 bits of the address and do a lookup. Then we also need
    // to handle the 4K aliasing problem.
    for (uint64_t k = tail; k < head; k++)
    {
        StoreBufferEntry &entry = entries[k % capacity];
        if (entry.addr == addr)
        {
            return optional<int>(entry.value);
        }
    }

    return nullopt;
}

bool StoreBuffer::is_empty()
{
    return head == tail;
}

void StoreBuffer::write(int addr, int value)
{
    StoreBufferEntry &entry = entries[tail % capacity];
    entry.value = value;
    entry.addr = addr;
    tail++;
}

void StoreBuffer::tick(vector<int> *memory)
{
    if (head != tail)
    {
        StoreBufferEntry &entry = entries[head % capacity];
        memory->at(entry.addr) = entry.value;
        printf("Writing to memory [%d]=%d\n", entry.addr, entry.value);
        head++;
    }
}

bool Frontend::tick()
{
    bool fetchNext;
    Slot *fetchSlot = &cpu->pipeline.slots[cpu->pipeline.index % PIPELINE_DEPTH];

    // Fetch
    if (bubbleSize > 0)
    {
        fetchNext = false;
        bubbleSize--;
        cpu->pipeline.slots[cpu->pipeline.index % PIPELINE_DEPTH].instr = cpu->nop;
    }
    else
    {
        fetchNext = true;
        Instr *instr = &cpu->code->at(cpu->ip);

        // when a branch enters the pipeline, the pipeline will be filled with nops
        // to prevent a control hazard. This will guarantee that the branch instruction
        // has been executed, before instructions of the taken or untaken branch are
        // added to the pipeline.
        if (instr->opcode == OPCODE_JNZ)
        {
            bubbleSize = PIPELINE_DEPTH - 1;
        }

        fetchSlot->instr = instr;
    }

    // Decode (ignored)

    return fetchNext;
}

void Backend::tick()
{
    // Execute
    Slot *executeSlot = &cpu->pipeline.slots[(cpu->pipeline.index + STAGE_EXECUTE) % PIPELINE_DEPTH];
    if (trace)
    {
        print_instr(executeSlot->instr);
    }
    execute(executeSlot->instr);
}

void Backend::execute(Instr *instr)
{
    switch (instr->opcode)
    {
        case OPCODE_ADD:
        {
            int v1 = arch_regs->at(instr->code.ADD.r_src1);
            int v2 = arch_regs->at(instr->code.ADD.r_src2);
            arch_regs->at(instr->code.ADD.r_dst) = v1 + v2;
            break;
        }
        case OPCODE_SUB:
        {
            int v1 = arch_regs->at(instr->code.SUB.r_src1);
            int v2 = arch_regs->at(instr->code.SUB.r_src2);
            arch_regs->at(instr->code.SUB.r_dst) = v1 + v2;
            break;
        }
        case OPCODE_AND:
        {
            int v1 = arch_regs->at(instr->code.AND.r_src1);
            int v2 = arch_regs->at(instr->code.AND.r_src2);
            arch_regs->at(instr->code.AND.r_dst) = v1 && v2;
            break;
        }
        case OPCODE_OR:
        {
            int v1 = arch_regs->at(instr->code.OR.r_src1);
            int v2 = arch_regs->at(instr->code.OR.r_src2);
            arch_regs->at(instr->code.OR.r_dst) = v1 || v2;
            break;
        }
        case OPCODE_NOT:
        {
            int v1 = arch_regs->at(instr->code.NOT.r_src);
            arch_regs->at(instr->code.OR.r_dst) = !v1;
            break;
        }
        case OPCODE_CMP:
        {
            int v1 = arch_regs->at(instr->code.CMP.r_src1);
            int v2 = arch_regs->at(instr->code.CMP.r_src2);
            arch_regs->at(instr->code.CMP.r_dst) = v1 == v2;
            break;
        }
        case OPCODE_INC:
        {
            arch_regs->at(instr->code.INC.r_src)++;
            break;
        }
        case OPCODE_DEC:
        {
            arch_regs->at(instr->code.DEC.r_src)--;
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
            int v1 = arch_regs->at(instr->code.PRINTR.r_src);
            printf("                                R%d=%d\n", instr->code.PRINTR.r_src, v1);
            break;
        }
        case OPCODE_JNZ:
        {
            int v1 = arch_regs->at(instr->code.JNZ.r_src);
            if (v1 != 0)
            {
                // the ip will be bumped at the end again, so -1 is subtracted
                cpu->ip = instr->code.JNZ.p_target - 1;
            }
            break;
        }
        case OPCODE_HALT:
        {
            // at the end of the cycle, the ip is bumped again and it will
            // end up as -1, and no further instructions will be processed.
            cpu->ip = -2;
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
