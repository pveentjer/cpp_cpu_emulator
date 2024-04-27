#include <iostream>
#include <vector>
#include <thread>

static const int REGISTER_COUNT = 32;
static const int MEMORY_SIZE = 16;
static const int STORE_BUFFER_CAPACITY = 4;
static const int CPU_FREQUENCY_HZ = 1;

static const int OPCODE_ADD = 1;
static const int OPCODE_INC = 2;
static const int OPCODE_LOAD = 3;
static const int OPCODE_STORE = 4;
static const int OPCODE_PRINT_REG = 5;
static const int OPCODE_HALT = 6;
static const int OPCODE_CMP = 7;
static const int OPCODE_JNZ = 8;
static const int OPCODE_DEC = 9;
static const int OPCODE_SUB = 10;
static const int OPCODE_AND = 11;
static const int OPCODE_OR = 12;
static const int OPCODE_NOT = 13;
// copy between registers
static const int OPCODE_MOV = 14;

struct Instruction
{
    int opcode;
    union
    {
        struct
        {
            int src1, src2, dst;
        } ADD;
        struct
        {
            int src1, src2, dst;
        } SUB;
        struct
        {
            int src1, src2, dst;
        } AND;
        struct
        {
            int src1, src2, dst;
        } OR;
        struct
        {
            int src, dst;
        } NOT;
        struct
        {
            int src, dst;
        } LOAD;
        struct
        {
            int src, dst;
        } STORE;
        struct
        {
            int src, dst;
        } MOV;
        struct
        {
            int src;
        } PRINT_REG;
        struct
        {
            int src;
        } INC;
        struct
        {
            int src;
        } DEC;
        struct
        {
        } HALT;
        struct
        {
            int src1, src2, dst;
        } CMP;
        struct
        {
            int src, target;
        } JNZ;
    } code;
};

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
};

class CPU
{

public:
    int32_t ip;
    std::vector<Instruction> *program;
    std::vector<int> *registers;
    std::vector<int> *memory;
    StoreBuffer sb;
    bool printInstructions;

    CPU()
    {
        ip = 0;
        program = new std::vector<Instruction>();
        registers = new std::vector<int>();
        for (int k = 0; k < REGISTER_COUNT; k++)
        {
            registers->push_back(0);
        }
        memory = new std::vector<int>();
        for (int k = 0; k < MEMORY_SIZE; k++)
        {
            memory->push_back(0);
        }
        sb.head = 0;
        sb.tail = 0;
        printInstructions = true;
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
                int a = registers->at(instr->code.ADD.src1);
                int b = registers->at(instr->code.ADD.src2);
                registers->at(instr->code.ADD.dst) = a + b;
                ip++;
                break;
            }
            case OPCODE_SUB:
            {
                int a = registers->at(instr->code.SUB.src1);
                int b = registers->at(instr->code.SUB.src2);
                registers->at(instr->code.SUB.dst) = a + b;
                ip++;
                break;
            }
            case OPCODE_AND:
            {
                int a = registers->at(instr->code.AND.src1);
                int b = registers->at(instr->code.AND.src2);
                registers->at(instr->code.AND.dst) = a && b;
                ip++;
                break;
            }
            case OPCODE_OR:
            {
                int a = registers->at(instr->code.OR.src1);
                int b = registers->at(instr->code.OR.src2);
                registers->at(instr->code.OR.dst) = a || b;
                ip++;
                break;
            }
            case OPCODE_NOT:
            {
                int a = registers->at(instr->code.NOT.src);
                registers->at(instr->code.OR.dst) = !a;
                ip++;
                break;
            }
            case OPCODE_CMP:
            {
                int a = registers->at(instr->code.CMP.src1);
                int b = registers->at(instr->code.CMP.src2);
                registers->at(instr->code.CMP.dst) = a == b;
                ip++;
                break;
            }
            case OPCODE_INC:
            {
                registers->at(instr->code.INC.src)++;
                ip++;
                break;
            }
            case OPCODE_DEC:
            {
                registers->at(instr->code.DEC.src)--;
                ip++;
                break;
            }
            case OPCODE_MOV:
            {
                registers->at(instr->code.MOV.dst) = registers->at(instr->code.MOV.src);
                ip++;
                break;
            }
            case OPCODE_LOAD:
            {
                registers->at(instr->code.LOAD.dst) = memory->at(instr->code.LOAD.src);
                ip++;
                break;
            }
            case OPCODE_STORE:
            {
                sb_add(registers->at(instr->code.STORE.src), instr->code.STORE.dst);
                ip++;
                break;
            }
            case OPCODE_PRINT_REG:
            {
                int a = registers->at(instr->code.PRINT_REG.src);
                printf("R%d=%d\n", instr->code.PRINT_REG.src, a);
                ip++;
                break;
            }
            case OPCODE_JNZ:
            {
                int a = registers->at(instr->code.JNZ.src);
                if (a != 0)
                {
                    ip = instr->code.JNZ.target;
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
                throw std::runtime_error("Unrecognized opcode=%d");
        }
    }

    void print(Instruction *instr)
    {
        switch (instr->opcode)
        {
            case OPCODE_ADD:
            {
                printf("ADD %d %d %d\n", instr->code.SUB.dst, instr->code.ADD.src1, instr->code.ADD.src2);
                break;
            }
            case OPCODE_SUB:
            {
                printf("SUB %d %d %d \n", instr->code.SUB.dst, instr->code.SUB.src1, instr->code.SUB.src2);
                break;
            }
            case OPCODE_AND:
            {
                printf("AND %d %d %d \n", instr->code.AND.dst, instr->code.AND.src1, instr->code.AND.src2);
                break;
            }
            case OPCODE_OR:
            {
                printf("OR %d %d %d \n", instr->code.OR.dst, instr->code.OR.src1, instr->code.OR.src2);
                break;
            }
            case OPCODE_NOT:
            {
                printf("NOT %d %d \n", instr->code.NOT.dst, instr->code.NOT.src);
                break;
            }
            case OPCODE_CMP:
            {
                printf("CMP %d %d %d \n", instr->code.CMP.dst, instr->code.CMP.src1, instr->code.CMP.src2);
                break;
            }
            case OPCODE_INC:
            {
                printf("INC %d \n", instr->code.INC.src);
                break;
            }
            case OPCODE_DEC:
            {
                printf("DEC %d \n", instr->code.DEC.src);
                break;
            }
            case OPCODE_MOV:
            {
                printf("MOV %d %d\n", instr->code.MOV.src, instr->code.MOV.dst);
                break;
            }
            case OPCODE_LOAD:
            {
                printf("LOAD %d %d\n", instr->code.LOAD.dst, instr->code.LOAD.src);
                break;
            }
            case OPCODE_STORE:
            {
                printf("STORE %d %d\n", instr->code.STORE.src, instr->code.STORE.dst);
                break;
            }
            case OPCODE_PRINT_REG:
            {
                printf("PRINT_REG %d\n", instr->code.PRINT_REG.src);
                break;
            }
            case OPCODE_JNZ:
            {
                printf("JNZ %d\n", instr->code.JNZ.src);
                break;
            }
            case OPCODE_HALT:
            {
                printf("HALT\n");
                break;
            }
            default:
                throw std::runtime_error("foobar");
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
        std::chrono::milliseconds duration(static_cast<int>(pause * 1000));
        std::this_thread::sleep_for(duration);

        if (ip > -1)
        {
            Instruction *instr = &program->at(ip);

            if(printInstructions)
            {
                print(instr);
            }

            execute(instr);
        }

        sb_tick();
    }

    void sb_add(int value, int addr)
    {
        StoreBufferEntry &entry = sb.entries[sb.tail % STORE_BUFFER_CAPACITY];
        entry.value = value;
        entry.addr = addr;
        sb.tail++;
    }

    void sb_tick()
    {
        if (sb.head != sb.tail)
        {
            StoreBufferEntry *entry = &sb.entries[sb.head % STORE_BUFFER_CAPACITY];
            memory->at(entry->addr) = entry->value;
            sb.head++;
        }
    }
};

int main()
{
    CPU *cpu = new CPU();
    cpu->memory->at(0) = 5;
    cpu->memory->at(1) = 20;

    cpu->program->push_back(Instruction());
    cpu->program->back().opcode = OPCODE_LOAD;
    cpu->program->back().code.LOAD.src = 0;
    cpu->program->back().code.LOAD.dst = 0;

    cpu->program->push_back(Instruction());
    cpu->program->back().opcode = OPCODE_LOAD;
    cpu->program->back().code.LOAD.src = 1;
    cpu->program->back().code.LOAD.dst = 1;

    cpu->program->push_back(Instruction());
    cpu->program->back().opcode = OPCODE_PRINT_REG;
    cpu->program->back().code.PRINT_REG.src = 0;

    cpu->program->push_back(Instruction());
    cpu->program->back().opcode = OPCODE_DEC;
    cpu->program->back().code.DEC.src = 0;

    cpu->program->push_back(Instruction());
    cpu->program->back().opcode = OPCODE_STORE;
    cpu->program->back().code.STORE.src = 0;
    cpu->program->back().code.STORE.dst = 0;

    cpu->program->push_back(Instruction());
    cpu->program->back().opcode = OPCODE_INC;
    cpu->program->back().code.DEC.src = 1;

    cpu->program->push_back(Instruction());
    cpu->program->back().opcode = OPCODE_STORE;
    cpu->program->back().code.STORE.src = 1;
    cpu->program->back().code.STORE.dst = 1;

    cpu->program->push_back(Instruction());
    cpu->program->back().opcode = OPCODE_JNZ;
    cpu->program->back().code.JNZ.src = 0;
    cpu->program->back().code.JNZ.target = 2;

    cpu->program->push_back(Instruction());
    cpu->program->back().opcode = OPCODE_HALT;

    while (!cpu->tick_again())
    {
        cpu->tick();
    }

    cpu->print_memory();
    return 0;
}
