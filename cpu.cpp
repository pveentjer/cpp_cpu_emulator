//
// Created by pveentjer on 4/30/24.
//

#include "cpu.h"


bool CPU::is_idle()
{
    return frontend.is_idle()
           && instr_queue.is_empty()
           //&& backend.is_empty()
           && sb.is_empty();
}

void CPU::cycle()
{
    frontend.cycle();

    backend.cycle();

    sb.cycle();

    cycles++;
}


void CPU::run()
{
    while (!is_idle())
    {
        this_thread::sleep_for(cycle_period_ms);
        cycle();
    }
}

void CPU::print_memory() const
{
    printf("------------------Memory----------------\n");
    for (int k = 0; k < memory->size(); k++)
    {
        printf("%04d %04d\n", k, memory->at(k));
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

void StoreBuffer::cycle()
{
    if (head != tail)
    {
        StoreBufferEntry &entry = entries[head % capacity];
        memory->at(entry.addr) = entry.value;
        head++;
    }
}

void Frontend::cycle()
{
    if (ip_next_fetch == -1)
    {
        return;
    }

    if (instr_queue->is_full())
    {
        return;
    }

    bool fetchNext;
    Instr *instr;
    // Fetch
    if (bubble_size > 0)
    {
        fetchNext = false;
        bubble_size--;
        instr = nop;
    }
    else
    {
        fetchNext = true;
        instr = &cpu->code->at(ip_next_fetch);

        // when a branch enters the pipeline, the pipeline will be filled with nops
        // to prevent a control hazard. This will guarantee that the branch instruction
        // has been executed, before instructions of the taken or untaken branch are
        // added to the pipeline.
        if (instr->opcode == OPCODE_JNZ)
        {
            bubble_size = PIPELINE_DEPTH - 1;
        }
    }

    instr_queue->enqueue(instr);

    if (fetchNext)
    {
        ip_next_fetch++;
    }
}

bool Frontend::is_idle()
{
    return ip_next_fetch == -1;
}

void Backend::cycle()
{
    if (is_idle())
    {
        return;
    }

    if (instr_queue->is_empty())
    {
        return;
    }

    Instr *instr = instr_queue->dequeue();
    if (trace)
    {
        print_instr(instr);
    }
    execute(instr);
}

bool Backend::is_idle()
{
    //todo: fix
    //return true;

    return false;
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
                cpu->frontend.ip_next_fetch = instr->code.JNZ.p_target;
            }
            break;
        }
        case OPCODE_HALT:
        {
            cpu->frontend.ip_next_fetch = -1;
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
