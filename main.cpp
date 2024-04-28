#include <iostream>
#include <vector>
#include <thread>
#include <optional>
#include <map>
#include <fstream>

static const int REGISTER_COUNT = 32;
static const int MEMORY_SIZE = 16;
static const int STORE_BUFFER_CAPACITY = 4;
static const int CPU_FREQUENCY_HZ = 1;

// adds 2 registers and writes the result in another
static const int OPCODE_ADD = 1;
// subtracts the second register from the first and writes the result in another
static const int OPCODE_SUB = 2;
// increments the value in a register
static const int OPCODE_INC = 3;
// decrements the value in a register
static const int OPCODE_DEC = 4;

static const int OPCODE_AND = 5;
static const int OPCODE_OR = 6;
static const int OPCODE_NOT = 7;

static const int OPCODE_LOAD = 8;
static const int OPCODE_STORE = 9;
// prints a value from a register
static const int OPCODE_PRINTR = 10;
static const int OPCODE_HALT = 11;
static const int OPCODE_CMP = 12;
static const int OPCODE_JNZ = 13;
// copy between registers
static const int OPCODE_MOV = 14;


std::map<std::string, int> mnemonicToOpcode = {
        {"ADD",    OPCODE_ADD},
        {"SUB",    OPCODE_SUB},
        {"AND",    OPCODE_AND},
        {"OR",     OPCODE_OR},
        {"NOT",    OPCODE_NOT},
        {"CMP",    OPCODE_CMP},
        {"MOV",    OPCODE_MOV},
        {"LOAD",   OPCODE_LOAD},
        {"STORE",  OPCODE_STORE},
        {"PRINTR", OPCODE_PRINTR},
        {"INC",    OPCODE_INC},
        {"DEC",    OPCODE_DEC},
        {"JNZ",    OPCODE_JNZ},
        {"HALT",   OPCODE_HALT}
};

struct Instruction
{
    int opcode;
    // r_ prefix means it is a register
    // m_prefix means it is from memory
    // p_prefix means it is an address in the program
    union
    {
        struct
        {
            uint32_t r_src1, r_src2, r_dst;
        } ADD;
        struct
        {
            uint32_t r_src1, r_src2, r_dst;
        } SUB;
        struct
        {
            uint32_t r_src1, r_src2, r_dst;
        } AND;
        struct
        {
            uint32_t r_src1, r_src2, r_dst;
        } OR;
        struct
        {
            uint32_t r_src, r_dst;
        } NOT;
        struct
        {
            uint32_t m_src, r_dst;
        } LOAD;
        struct
        {
            uint32_t r_src, m_dst;
        } STORE;
        struct
        {
            uint32_t r_src, r_dst;
        } MOV;
        struct
        {
            uint32_t r_src;
        } PRINTR;
        struct
        {
            uint32_t r_src;
        } INC;
        struct
        {
            uint32_t r_src;
        } DEC;
        struct
        {
        } HALT;
        struct
        {
            uint32_t r_src1, r_src2, r_dst;
        } CMP;
        struct
        {
            uint32_t r_src, p_target;
        } JNZ;
    } code;
};

void print_instr(Instruction *instr)
{
    switch (instr->opcode)
    {
        case OPCODE_ADD:
        {
            printf("ADD %d %d %d\n", instr->code.ADD.r_dst, instr->code.ADD.r_src1, instr->code.ADD.r_src2);
            break;
        }
        case OPCODE_SUB:
        {
            printf("SUB %d %d %d \n", instr->code.SUB.r_dst, instr->code.SUB.r_src1, instr->code.SUB.r_src2);
            break;
        }
        case OPCODE_AND:
        {
            printf("AND %d %d %d \n", instr->code.AND.r_dst, instr->code.AND.r_src1, instr->code.AND.r_src2);
            break;
        }
        case OPCODE_OR:
        {
            printf("OR %d %d %d \n", instr->code.OR.r_dst, instr->code.OR.r_src1, instr->code.OR.r_src2);
            break;
        }
        case OPCODE_NOT:
        {
            printf("NOT %d %d \n", instr->code.NOT.r_dst, instr->code.NOT.r_src);
            break;
        }
        case OPCODE_CMP:
        {
            printf("CMP %d %d %d \n", instr->code.CMP.r_dst, instr->code.CMP.r_src1, instr->code.CMP.r_src2);
            break;
        }
        case OPCODE_INC:
        {
            printf("INC %d \n", instr->code.INC.r_src);
            break;
        }
        case OPCODE_DEC:
        {
            printf("DEC %d \n", instr->code.DEC.r_src);
            break;
        }
        case OPCODE_MOV:
        {
            printf("MOV %d %d\n", instr->code.MOV.r_src, instr->code.MOV.r_dst);
            break;
        }
        case OPCODE_LOAD:
        {
            printf("LOAD %d %d\n", instr->code.LOAD.r_dst, instr->code.LOAD.m_src);
            break;
        }
        case OPCODE_STORE:
        {
            printf("STORE %d %d\n", instr->code.STORE.r_src, instr->code.STORE.m_dst);
            break;
        }
        case OPCODE_PRINTR:
        {
            printf("PRINTR %d\n", instr->code.PRINTR.r_src);
            break;
        }
        case OPCODE_JNZ:
        {
            printf("JNZ %d\n", instr->code.JNZ.r_src);
            break;
        }
        case OPCODE_HALT:
        {
            printf("HALT\n");
            break;
        }
        default:
            throw std::runtime_error("Unrecognized opcode");
    }
}


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
    // when true, prints every instruction before being executed.
    bool trace;

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
        trace = false;
    }

    int load_program(std::string file)
    {
        // Open the file containing the program instructions
        std::ifstream infile(file);
        if (!infile.is_open())
        {
            std::cerr << "Failed to open program file." << std::endl;
            return 1;
        }

        uint32_t line = 0;
        std::string mnemonic;
        while (infile >> mnemonic)
        {
            line++;
            Instruction instr;
            printf("mnemonic %s\n", mnemonic.c_str());
            auto it = mnemonicToOpcode.find(mnemonic);
            if (it != mnemonicToOpcode.end())
            {
                instr.opcode = it->second;

                switch (instr.opcode)
                {

                    case OPCODE_AND:
                    {
                        int r_src1, r_src2;
                        if (infile >> r_src1 >> r_src2)
                        {
                            instr.code.AND.r_src1 = r_src1;
                            instr.code.AND.r_src2 = r_src2;
                        }
                        else
                        {
                            std::cerr << "Invalid AND instruction format at line " << line << "." << std::endl;
                            return 1;
                        }
                        break;
                    }
                    case OPCODE_OR:
                    {
                        int r_src1, r_src2;
                        if (infile >> r_src1 >> r_src2)
                        {
                            instr.code.OR.r_src1 = r_src1;
                            instr.code.OR.r_src2 = r_src2;
                        }
                        else
                        {
                            std::cerr << "Invalid OR instruction format at line " << line << "." << std::endl;
                            return 1;
                        }
                        break;
                    }
                    case OPCODE_NOT:
                    {
                        int r_src;
                        if (infile >> r_src)
                        {
                            instr.code.NOT.r_src = r_src;
                        }
                        else
                        {
                            std::cerr << "Invalid NOT instruction format at line " << line << "." << std::endl;
                            return 1;
                        }
                        break;
                    }
                    case OPCODE_ADD:
                    {
                        int r_src1, r_src2;
                        if (infile >> r_src1 >> r_src2)
                        {
                            instr.code.ADD.r_src1 = r_src1;
                            instr.code.ADD.r_src2 = r_src2;
                        }
                        else
                        {
                            std::cerr << "Invalid ADD instruction format at line " << line << "." << std::endl;
                            return 1;
                        }
                        break;
                    }
                    case OPCODE_SUB:
                    {
                        int r_src1, r_src2;
                        if (infile >> r_src1 >> r_src2)
                        {
                            instr.code.SUB.r_src1 = r_src1;
                            instr.code.SUB.r_src2 = r_src2;
                        }
                        else
                        {
                            std::cerr << "Invalid SUB instruction format at line " << line << "." << std::endl;
                            return 1;
                        }
                        break;
                    }
                    case OPCODE_DEC:
                    {
                        int r_src;
                        if (infile >> r_src)
                        {
                            instr.code.DEC.r_src = r_src;
                        }
                        else
                        {
                            std::cerr << "Invalid DEC instruction format at line " << line << "." << std::endl;
                            return 1;
                        }
                        break;
                    }
                    case OPCODE_INC:
                    {
                        int r_src;
                        if (infile >> r_src)
                        {
                            instr.code.INC.r_src = r_src;
                        }
                        else
                        {
                            std::cerr << "Invalid INC instruction format at line " << line << "." << std::endl;
                            return 1;
                        }
                        break;
                    }
                    case OPCODE_LOAD:
                    {
                        int m_src, r_dst;
                        if (infile >> m_src >> r_dst)
                        {
                            instr.code.LOAD.m_src = m_src;
                            instr.code.LOAD.r_dst = r_dst;
                        }
                        else
                        {
                            std::cerr << "Invalid LOAD instruction format at line " << line << "." << std::endl;
                            return 1;
                        }
                        break;
                    }
                    case OPCODE_STORE:
                    {
                        int r_src, m_dst;
                        if (infile >> r_src >> m_dst)
                        {
                            instr.code.STORE.r_src = r_src;
                            instr.code.STORE.m_dst = m_dst;
                        }
                        else
                        {
                            std::cerr << "Invalid STORE instruction format at line " << line << "." << std::endl;
                            return 1;
                        }
                        break;
                    }
                    case OPCODE_JNZ:
                    {
                        int r_src, p_target;
                        if (infile >> r_src >> p_target)
                        {
                            instr.code.JNZ.r_src = r_src;
                            instr.code.JNZ.p_target = p_target;
                        }
                        else
                        {
                            std::cerr << "Invalid JNZ instruction format at line " << line << "." << std::endl;
                            return 1;
                        }
                        break;
                    }
                    case OPCODE_PRINTR:
                    {
                        int r_src;
                        if (infile >> r_src)
                        {
                            instr.code.PRINTR.r_src = r_src;
                        }
                        else
                        {
                            std::cerr << "Invalid PRINTR instruction format at line " << line << "." << std::endl;
                            return 1;
                        }
                        break;
                    }
                }

                program->push_back(instr);
            }
            else
            {
                std::cerr << "Unknown mnemonic: " << mnemonic << std::endl;
                return 1;
            }
        }

        // todo: file is not closed on error.
        // Close the file
        infile.close();
        return 0;
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

                int value = sb_lookup(instr->code.LOAD.m_src)
                        .value_or(memory->at(instr->code.LOAD.m_src));

                registers->at(instr->code.LOAD.r_dst) = value;
                ip++;
                break;
            }
            case OPCODE_STORE:
            {
                sb_write(instr->code.STORE.m_dst, registers->at(instr->code.STORE.r_src));
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
                throw std::runtime_error("Unrecognized opcode");
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
        std::chrono::milliseconds period(static_cast<int>(pause * 1000));
        std::this_thread::sleep_for(period);

        if (ip > -1)
        {
            Instruction *instr = &program->at(ip);

            if (trace)
            {
                print_instr(instr);
            }

            execute(instr);
        }

        sb_tick();
    }

    // todo: move to sb.
    std::optional<int> sb_lookup(int addr)
    {
        // todo: instead of iterating over all values, there should be a directly-mapped hash-table
        // so that we can use the last 12 bits of the address and do a lookup. Then we also need
        // to handle the 4K aliasing problem.
        for (uint64_t k = sb.tail; k < sb.head; k++)
        {
            StoreBufferEntry &entry = sb.entries[k % STORE_BUFFER_CAPACITY];
            if (entry.addr == addr)
            {
                return std::optional<int>(entry.value);
            }
        }

        return std::nullopt;
    }

    // todo: move to sb
    void sb_write(int addr, int value)
    {
        StoreBufferEntry &entry = sb.entries[sb.tail % STORE_BUFFER_CAPACITY];
        entry.value = value;
        entry.addr = addr;
        sb.tail++;
    }

    // todo: move to sb.
    void sb_tick()
    {
        if (sb.head != sb.tail)
        {
            StoreBufferEntry &entry = sb.entries[sb.head % STORE_BUFFER_CAPACITY];
            memory->at(entry.addr) = entry.value;
            sb.head++;
        }
    }
};

int main()
{
    CPU *cpu = new CPU();
    cpu->trace = true;
    cpu->memory->at(0) = 5;
    cpu->memory->at(1) = 20;

    int res = cpu->load_program("program.txt");
    if (res != 0)
    {
        return -1;
    }

    while (!cpu->tick_again())
    {
        cpu->tick();
    }

    cpu->print_memory();
    return 0;
}
