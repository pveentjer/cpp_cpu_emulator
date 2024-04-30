//
// Created by pveentjer on 4/30/24.
//

#ifndef CPU_EMULATOR_CPU_H
#define CPU_EMULATOR_CPU_H

#include <iostream>
#include <vector>
#include <thread>
#include <optional>
#include <map>
#include <fstream>
#include <algorithm>
#include <sstream>
#include "instructions.h"
#include "utils.h"

static const int REGISTER_COUNT = 32;
static const int MEMORY_SIZE = 16;
static const int STORE_BUFFER_CAPACITY = 4;
static const int CPU_FREQUENCY_HZ = 1;

using namespace std;

struct StoreBufferEntry
{
    int value;
    int addr;
};

struct StoreBuffer
{
    StoreBufferEntry entries[STORE_BUFFER_CAPACITY];
    uint64_t head;
    uint64_t tail;

    optional<int> lookup(int addr)
    {
        // todo: instead of iterating over all values, there should be a directly-mapped hash-table
        // so that we can use the last 12 bits of the address and do a lookup. Then we also need
        // to handle the 4K aliasing problem.
        for (uint64_t k = tail; k < head; k++)
        {
            StoreBufferEntry &entry = entries[k % STORE_BUFFER_CAPACITY];
            if (entry.addr == addr)
            {
                return optional<int>(entry.value);
            }
        }

        return nullopt;
    }

    void write(int addr, int value)
    {
        StoreBufferEntry &entry = entries[tail % STORE_BUFFER_CAPACITY];
        entry.value = value;
        entry.addr = addr;
        tail++;
    }

    void tick(vector<int> *memory)
    {
        if (head != tail)
        {
            StoreBufferEntry &entry = entries[head % STORE_BUFFER_CAPACITY];
            memory->at(entry.addr) = entry.value;
            head++;
        }
    }
};



using namespace std;

class CPU
{

public:
    int32_t ip;
    vector<Instruction> *program;
    vector<int> *registers;
    vector<int> *memory;
    StoreBuffer sb;
    // when true, prints every instruction before being executed.
    bool trace;

    CPU()
    {
        ip = 0;
        program = new vector<Instruction>();
        registers = new vector<int>();
        for (int k = 0; k < REGISTER_COUNT; k++)
        {
            registers->push_back(0);
        }
        memory = new vector<int>();
        for (int k = 0; k < MEMORY_SIZE; k++)
        {
            memory->push_back(0);
        }
        sb.head = 0;
        sb.tail = 0;
        trace = false;
    }


    void print_memory()
    {
        printf("------------------Memory----------------\n");
        for (int k = 0; k < memory->size(); k++)
        {
            printf("%04d %04d\n", k, memory->at(k));
        }
    }

    void execute(Instruction *instr)
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

    bool tick_again() const
    {
        if (ip > -1)
        {
            return false;
        }
        return sb.head == sb.tail;
    }

    void tick()
    {
        double pause = 1.0 / CPU_FREQUENCY_HZ;
        chrono::milliseconds period(static_cast<int>(pause * 1000));
        this_thread::sleep_for(period);

        if (ip > -1)
        {
            Instruction *instr = &program->at(ip);

            if (trace)
            {
                print_instr(instr);
            }

            execute(instr);
        }

        sb.tick(memory);
    }
};

#endif //CPU_EMULATOR_CPU_H
