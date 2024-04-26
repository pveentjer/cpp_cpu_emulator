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

struct Instruction {
    int opcode;
    union {
        struct {
            int src1, src2, dst;
        } add;
        struct {
            int src, dst;
        } load;
        struct {
            int src, dst;
        } store;
        struct {
            int src;
        } print;
        struct {
            int src;
        } inc;
        struct {
        } halt;
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
                int a = registers->at(instr->code.add.src1);
                int b = registers->at(instr->code.add.src2);
                registers->at(instr->code.add.dst) = a + b;
                ip++;
                break;
            }
            case OPCODE_INC: {
                int v = registers->at(instr->code.inc.src);
                registers->at(instr->code.inc.src) = v + 1;
                ip++;
                break;
            }
            case OPCODE_LOAD: {
                registers->at(instr->code.load.dst) = memory->at(instr->code.load.src);
                ip++;
                break;
            }
            case OPCODE_STORE: {
                memory->at(instr->code.store.dst) = registers->at(instr->code.store.src);
                ip++;
                break;
            }
            case OPCODE_PRINT: {
                int a = registers->at(instr->code.print.src);
                printf("print %d\n", a);
                ip++;
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
        printf("tick %d\n", ip);
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
    cpu->program->back().code.load.src = 0;
    cpu->program->back().code.load.dst = 0;

    cpu->program->push_back(Instruction());
    cpu->program->back().opcode = OPCODE_INC;
    cpu->program->back().code.inc.src = 0;

    cpu->program->push_back(Instruction());
    cpu->program->back().opcode = OPCODE_PRINT;
    cpu->program->back().code.print.src = 0;

    cpu->program->push_back(Instruction());
    cpu->program->back().opcode = OPCODE_HALT;

    while (!cpu->has_halted()) {
        cpu->tick();
    }

    cpu->print_memory();
    return 0;
}
