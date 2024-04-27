#include <iostream>
#include <vector>

static const int REGISTER_COUNT = 32;
static const int MEMORY_SIZE = 16;

static const int OPCODE_ADD = 1;
static const int OPCODE_INC = 2;
static const int OPCODE_LOAD = 3;
static const int OPCODE_STORE = 4;
static const int OPCODE_PRINT = 5;
static const int OPCODE_HALT = 6;
static const int OPCODE_CMP = 7;
static const int OPCODE_JNZ = 8;
static const int OPCODE_DEC = 9;
static const int OPCODE_SUB = 10;
static const int OPCODE_AND = 11;
static const int OPCODE_OR = 12;
static const int OPCODE_NOT = 13;

struct Instruction {
    int opcode;
    union {
        struct {
            int src1, src2, dst;
        } ADD;
        struct {
            int src1, src2, dst;
        } SUB;
        struct {
            int src1, src2, dst;
        } AND;
        struct {
            int src1, src2, dst;
        } OR;
        struct {
            int src, dst;
        } NOT;
        struct {
            int src, dst;
        } LOAD;
        struct {
            int src, dst;
        } STORE;
        struct {
            int src;
        } PRINT_REG;
        struct {
            int src;
        } INC;
        struct {
            int src;
        } DEC;
        struct {
        } HALT;
        struct {
            int sr1, src2, dst;
        } CMP;
        struct {
            int src, target;
        } JNZ;
    } code;
};

class CPU {

public:
    int32_t ip;
    std::vector<Instruction> *program;
    std::vector<int> *registers;
    std::vector<int> *memory;

    CPU() {
        ip = 0;
        program = new std::vector<Instruction>();
        registers = new std::vector<int>();
        for (int k = 0; k < REGISTER_COUNT; k++) {
            registers->push_back(0);
        }
        memory = new std::vector<int>();
        for (int k = 0; k < MEMORY_SIZE; k++) {
            memory->push_back(0);
        }
    }

    void print_memory() {
        printf("------------------Memory----------------\n");
        for (int k = 0; k < memory->size(); k++) {
            printf("%04d %04d\n", k, memory->at(k));
        }
    }

    void execute(Instruction *instr) {
        switch (instr->opcode) {
            case OPCODE_ADD: {
                int a = registers->at(instr->code.ADD.src1);
                int b = registers->at(instr->code.ADD.src2);
                registers->at(instr->code.ADD.dst) = a + b;
                ip++;
                break;
            }
            case OPCODE_SUB: {
                int a = registers->at(instr->code.SUB.src1);
                int b = registers->at(instr->code.SUB.src2);
                registers->at(instr->code.SUB.dst) = a + b;
                ip++;
                break;
            }
            case OPCODE_AND: {
                int a = registers->at(instr->code.AND.src1);
                int b = registers->at(instr->code.AND.src2);
                registers->at(instr->code.AND.dst) = a && b;
                ip++;
                break;
            }
            case OPCODE_OR: {
                int a = registers->at(instr->code.OR.src1);
                int b = registers->at(instr->code.OR.src2);
                registers->at(instr->code.OR.dst) = a || b;
                ip++;
                break;
            }
            case OPCODE_NOT: {
                int a = registers->at(instr->code.NOT.src);
                registers->at(instr->code.OR.dst) = !a;
                ip++;
                break;
            }
            case OPCODE_CMP: {
                int a = registers->at(instr->code.ADD.src1);
                int b = registers->at(instr->code.ADD.src2);
                registers->at(instr->code.ADD.dst) = a == b;
                ip++;
                break;
            }
            case OPCODE_INC: {
                registers->at(instr->code.INC.src)++;
                ip++;
                break;
            }
            case OPCODE_DEC: {
                registers->at(instr->code.DEC.src)--;
                ip++;
                break;
            }
            case OPCODE_LOAD: {
                registers->at(instr->code.LOAD.dst) = memory->at(instr->code.LOAD.src);
                ip++;
                break;
            }
            case OPCODE_STORE: {
                memory->at(instr->code.STORE.dst) = registers->at(instr->code.STORE.src);
                ip++;
                break;
            }
            case OPCODE_PRINT: {
                int a = registers->at(instr->code.PRINT_REG.src);
                printf("%d\n", a);
                ip++;
                break;
            }
            case OPCODE_JNZ: {
                int a = registers->at(instr->code.JNZ.src);
                if (a != 0) {
                    ip = instr->code.JNZ.target;
                } else {
                    ip++;
                }
                break;
            }
            case OPCODE_HALT: {
                ip = -1;
                break;
            }
            default:
                throw std::runtime_error("foobar");
        }
    }

    bool has_halted() const {
        return ip == -1;
    }

    void tick() {
//printf("tick %d\n", ip);
        Instruction *instr = &program->at(ip);
        execute(instr);
    }
};

int main() {
    CPU *cpu = new CPU();
    cpu->memory->at(0) = 5;
    cpu->memory->at(1) = 10;
    cpu->memory->at(2) = 20;
    cpu->program->push_back(Instruction());
    cpu->program->back().opcode = OPCODE_LOAD;
    cpu->program->back().code.LOAD.src = 0;
    cpu->program->back().code.LOAD.dst = 0;

    cpu->program->push_back(Instruction());
    cpu->program->back().opcode = OPCODE_PRINT;
    cpu->program->back().code.PRINT_REG.src = 0;

    cpu->program->push_back(Instruction());
    cpu->program->back().opcode = OPCODE_DEC;
    cpu->program->back().code.DEC.src = 0;

    cpu->program->push_back(Instruction());
    cpu->program->back().opcode = OPCODE_JNZ;
    cpu->program->back().code.JNZ.src = 0;
    cpu->program->back().code.JNZ.target = 1;

    cpu->program->push_back(Instruction());
    cpu->program->back().opcode = OPCODE_HALT;

    while (!cpu->has_halted()) {
        cpu->tick();
    }

    cpu->print_memory();
    return 0;
}
