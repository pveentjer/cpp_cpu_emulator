#ifndef CPU_EMULATOR_INSTRUCTIONS_H
#define CPU_EMULATOR_INSTRUCTIONS_H

#include <cstdint>
#include <cstdio>
#include <stdexcept>
#include <map>
#include <unordered_map>

static const int MAX_INPUT_OPERANDS = 3;
static const int MAX_OUTPUT_OPERANDS = 1;

enum Opcode
{
    OPCODE_ADD,
    OPCODE_SUB,
    OPCODE_INC,
    OPCODE_DEC,
    OPCODE_AND,
    OPCODE_OR,
    OPCODE_XOR,
    OPCODE_NOT,
    OPCODE_LOAD,
    OPCODE_STORE,
    OPCODE_PRINTR,
    OPCODE_HALT,
    OPCODE_CMP,
    OPCODE_JNZ,
    OPCODE_MOV,
    OPCODE_NOP
};

bool is_branch(Opcode opcode);

static const std::unordered_map<std::string, Opcode> MNEMONIC_TO_OPCODE = {
        {"ADD",    OPCODE_ADD},
        {"SUB",    OPCODE_SUB},
        {"AND",    OPCODE_AND},
        {"OR",     OPCODE_OR},
        {"XOR",    OPCODE_XOR},
        {"NOT",    OPCODE_NOT},
        {"CMP",    OPCODE_CMP},
        {"MOV",    OPCODE_MOV},
        {"LOAD",   OPCODE_LOAD},
        {"STORE",  OPCODE_STORE},
        {"PRINTR", OPCODE_PRINTR},
        {"INC",    OPCODE_INC},
        {"DEC",    OPCODE_DEC},
        {"JNZ",    OPCODE_JNZ},
        {"HALT",   OPCODE_HALT},
        {"NOP",    OPCODE_NOP}
};

enum OperandType {
    REGISTER,
    MEMORY,
    CODE,
    CONSTANT,
};

struct Operand {
    OperandType type;
    union {
        uint16_t reg;
        uint64_t memory_addr;
        int constant;
        uint64_t code_addr;
    };
};


struct Instr
{
    Opcode opcode;

    uint16_t input_ops_cnt;
    Operand input_ops[MAX_INPUT_OPERANDS];

    uint16_t output_ops_cnt;
    Operand output_ops[MAX_OUTPUT_OPERANDS];
};

void print_instr(Instr *instr);


/**
 * The InstrQueue sits between frontend and backend.
 */
struct InstrQueue
{
    Instr **entries;
    uint16_t capacity;
    uint64_t head = 0;
    uint64_t tail = 0;

    InstrQueue(uint16_t capacity):capacity(capacity){
        head = 0;
        tail = 0;
        entries = new Instr *[capacity];
    }

    ~InstrQueue(){
        delete[] entries;
    }

    [[nodiscard]] bool is_empty() const
    {
        return head == tail;
    }

    [[nodiscard]] uint16_t size() const
    {
        return tail - head;
    }

    [[nodiscard]] bool is_full() const
    {
        return size() == capacity;
    }

    Instr *dequeue()
    {
        Instr *instr = entries[head % capacity];
        head++;
        return instr;
    }

    void enqueue(Instr *instr)
    {
        entries[tail % capacity] = instr;
        tail++;
    }
};


#endif //CPU_EMULATOR_INSTRUCTIONS_H
