#include <iostream>
#include <vector>

static const int REGISTER_COUNT = 32;
static const int MEMORY_SIZE = 16;

class CPU;

class Instruction {
public:
    virtual void execute(CPU *cpu) = 0;
};

class CPU {

public:
    int32_t ip;
    std::vector<Instruction *> *program;
    std::vector<int> *registers;
    std::vector<int> *memory;

    CPU() {
        ip = 0;
        program = new std::vector<Instruction *>();
        registers = new std::vector<int>();
        for (int k = 0; k < REGISTER_COUNT; k++) {
            registers->push_back(0);
        }
        memory = new std::vector<int>();
        for (int k = 0; k < MEMORY_SIZE; k++) {
            memory->push_back(0);
        }
    }

    void print_memory(){
        printf("------------------Memory----------------\n");
        for(int k=0;k<memory->size();k++){
            printf("%04d %04d\n",k, memory->at(k));
        }
    }

    bool has_halted() const {
        return ip == -1;
    }

    void tick() {
        printf("tick %d\n", ip);
        Instruction *instruction = program->at(ip);
        instruction->execute(this);
    }
};

class Add : public Instruction {

public:

    int src_1, src_2, dst;

    Add(int src_1, int src_2, int dst) : src_1(src_1), src_2(src_2), dst(dst) {
    }

    void execute(CPU *cpu) override {
        int a = cpu->registers->at(src_1);
        int b = cpu->registers->at(src_2);
        cpu->registers->at(dst) = a + b;
        cpu->ip++;
    }
};

class Load : public Instruction {

public:

    int src, dst;

    Load(int src, int dst) : src(src), dst(dst) {
    }

    void execute(CPU *cpu) override {
        cpu->registers->at(dst) = cpu->memory->at(src);
        cpu->ip++;
    }
};


class Inc: public Instruction {
public:

    int src, dst;

    Inc(int src) : src(src) {
    }

    void execute(CPU *cpu) override {
        int v = cpu->registers->at(src);
        cpu->registers->at(src)=v+1;
        cpu->ip++;
    }
};

class Store : public Instruction {

public:

    int src, dst;

    Store(int src, int dst) : src(src), dst(dst) {
    }

    void execute(CPU *cpu) override {
        cpu->memory->at(src) = cpu->registers->at(dst);
        cpu->ip++;
    }
};

class Halt : public Instruction {
    void execute(CPU *cpu) override {
        cpu->ip = -1;
        printf("halt\n");
    }
};

class Print : public Instruction {
public:
    int src;

    Print(int src) : src(src) {

    }

    void execute(CPU *cpu) override {
        int a = cpu->registers->at(src);
        printf("print %d\n", a);
        cpu->ip++;
    }
};


int main() {
    CPU *cpu = new CPU();
    cpu->memory->at(0) = 5;
    cpu->memory->at(1) = 10;
    cpu->memory->at(2) = 20;

    cpu->program->push_back(new Load(0,0));
    cpu->program->push_back(new Inc(0));
    cpu->program->push_back(new Load(1,1));
    cpu->program->push_back(new Load(2,2));

    cpu->program->push_back(new Add(0, 1, 3));
    cpu->program->push_back(new Add(2, 3, 4));

    cpu->program->push_back(new Store(4,1));
    cpu->program->push_back(new Halt());

    while (!cpu->has_halted()) {
        cpu->tick();
    }

    cpu->print_memory();
    return 0;
}
