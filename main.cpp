#include <iostream>
#include <vector>

static const int REGISTER_COUNT = 32;

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

    CPU() {
        ip = 0;
        program = new std::vector<Instruction *>();
        registers = new std::vector<int>();
        for (int k = 0; k < REGISTER_COUNT; k++) {
            registers->push_back(0);
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

public:

    void execute(CPU *cpu) override {
        printf("add\n");
        int a = cpu->registers->at(src_1);
        int b = cpu->registers->at(src_2);
        cpu->registers->at(dst) = a + b;
        cpu->ip++;
    }
};

class Halt : public Instruction {
    void execute(CPU *cpu) override {
        cpu->ip = -1;
        printf("halt");
    }
};

class Print : public Instruction {
public:
    int src;

    void execute(CPU *cpu) override {
        int a = cpu->registers->at(src);
        printf("print %d\n", a);
        cpu->ip++;
    }
};


int main() {
    CPU *cpu = new CPU();
    Add *add = new Add();
    add->src_1 = 0;
    add->src_2 = 1;
    add->dst = 2;
    cpu->program->push_back(add);

    Print *print = new Print();
    print->src = 2;

    cpu->program->push_back(print);
    cpu->program->push_back(new Halt());

    cpu->registers->at(0)=5;
    cpu->registers->at(1)=10;

    while (!cpu->has_halted()) {
        cpu->tick();
    }
    return 0;
}
