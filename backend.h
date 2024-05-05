//
// Created by pveentjer on 5/3/24.
//

#ifndef CPU_EMULATOR_BACKEND_H
#define CPU_EMULATOR_BACKEND_H


#include "memory_subsystem.h"
#include "frontend.h"

enum ROB_Slot_State
{
    ROB_SLOT_FREE,
    ROB_SLOT_EXECUTED
};

struct RS;
struct RS_Table;

struct ROB_Slot
{
    Instr *instr;
    int result;
    ROB_Slot_State state;
    RS *rs;
};

struct ROB
{
    uint64_t head, tail, reserved;
    uint16_t capacity;
    ROB_Slot *slots;


    ROB(uint16_t capacity) : capacity(capacity)
    {
        head = 0;
        tail = 0;
        reserved = 0;
        slots = new ROB_Slot[capacity];
        for (int k = 0; k < capacity; k++)
        {
            ROB_Slot &rob_slot = slots[k];
            rob_slot.instr = nullptr;
            rob_slot.rs = nullptr;
            rob_slot.result = 0;
            rob_slot.state = ROB_SLOT_FREE;
        }
    }

    ~ROB()
    {
        delete[] slots;
    }

    uint16_t empty_slots()
    {
        return capacity - size();
    }

    uint16_t size()
    {
        return tail - head;
    }
};


struct Backend;


struct ExecutionUnit
{
    RS *rs;
    Backend *backend;
    // the input in_operands
    Operand in_operands[MAX_INPUT_OPERANDS];
    // the result: probably this should also be an operand.
    int result;

    void execute();
};


struct RAT_Entry
{
    uint16_t phys_reg;
    // True of this entry is currently in use.
    bool valid;
};

// the register alias table used for register renaming
struct RAT
{
    RAT_Entry *entries;

    RAT(int arg_reg_cnt)
    {
        entries = new RAT_Entry[arg_reg_cnt];
        for (int k = 0; k < arg_reg_cnt; k++)
        {
            entries[k].phys_reg = k;
            entries[k].valid = true;
        }
    }

    ~RAT()
    {
        delete[] entries;
    }
};


struct Phys_Reg_Slot
{
    int value;
    bool has_value;
};

struct Phys_Reg_File
{
    uint16_t count;
    Phys_Reg_Slot *array;
    uint16_t *free_stack;
    uint16_t free_stack_size;

    Phys_Reg_File(uint16_t phys_reg_count)
    {
        count = phys_reg_count;
        free_stack = new uint16_t[phys_reg_count];
        for (uint16_t k = 0; k < phys_reg_count; k++)
        {
            free_stack[k] = k;
        }
        free_stack_size = phys_reg_count;
        array = new Phys_Reg_Slot[phys_reg_count];
        for (int k = 0; k < phys_reg_count; k++)
        {
            Phys_Reg_Slot &phys_reg = array[k];
            phys_reg.value = 0;
            phys_reg.has_value = false;
        }
    }

    ~Phys_Reg_File()
    {
        delete[] array;
        delete[] free_stack;
    }

    /**
     * Allocates a physical register.
     *
     * @return the physical register.
     */
    uint16_t allocate();

    /**
     * Deallocates a physical register.
     *
     * @param phys_reg the physical register to deallocate.
     */
    void deallocate(uint16_t phys_reg);
};


/**
 * The Backend is responsible for the actual execution of the instruction.
 *
 * It will take instructions from the InstrQueue.
 */
struct Backend
{
    RS_Table *rs_table;
    Frontend *frontend;
    // when true, prints every instruction before being executed.
    bool trace;
    int *arch_regs;
    StoreBuffer *sb;
    vector<int> *memory;
    InstrQueue *instr_queue;
    ROB *rob;
    ExecutionUnit eu;
    RAT *rat;
    Phys_Reg_File *phys_reg_file;

    void cycle();

    bool is_idle();

    void on_rs_ready(RS *rs);

    void retire(ROB_Slot *rob_slot);

    void cycle_retire();

    void cycle_dispatch();

    void cycle_issue();

    void cdb_broadcast(uint16_t phys_reg, int result);

};

enum RS_State
{
    RS_FREE,
    // The RS is waiting for 1 or more of its src_phys_registers in_operands.
    RS_ISSUED,
    // The RS has all in_operands ready, but not yet submitted
    RS_READY,
    // not used yet; but as soon as we set a queue between the RS and EU, this is needed
    RS_SUBMITTED,
    // The instruction has executed.
    RS_COMPLETED,
};

// A reservation station
struct RS
{

    RS_State state;

    uint16_t rs_index;

    Operand input_ops[MAX_INPUT_OPERANDS];

    // number of required operands.
    int input_op_cnt;

    // the number of operands that are ready
    int input_opt_ready_cnt;

    Operand output_ops[MAX_OUTPUT_OPERANDS];

    ROB_Slot *rob_slot;
};

struct RS_Table
{
    // the array with the reservation stations
    RS *array;
    // the number of reservation stations
    uint16_t count;

    // A stack of free RS.
    uint16_t *free_stack;
    uint16_t free_stack_size;

    // A circular queue with RS that are ready to be submitted
    uint16_t *ready_queue;
    uint64_t ready_tail, ready_head;

    RS_Table(uint16_t rs_count) : count(rs_count)
    {
        array = new RS[rs_count];
        for (uint16_t k = 0; k < rs_count; k++)
        {
            RS &rs = array[k];
            rs.rs_index = k;
            rs.state = RS_FREE;
        }
        free_stack_size = rs_count;
        free_stack = new uint16_t[rs_count];
        for (uint16_t k = 0; k < rs_count; k++)
        {
            free_stack[k] = k;
        }

        ready_head = 0;
        ready_tail = 0;
        ready_queue = new uint16_t[rs_count];
    }

    ~RS_Table()
    {
        delete[] free_stack;
        delete[] array;
        delete[] ready_queue;
    }

    optional<RS *> allocate()
    {
        if (free_stack_size == 0)
        {
            // There are no free reservation stations, so we are done
            return nullopt;
        }

        // get a free RS
        free_stack_size--;
        return &array[free_stack[free_stack_size]];
    }

    void deallocate(RS *rs)
    {
        if (free_stack_size == count)
        {
            throw std::runtime_error("RS_Table: too many frees");
        }

        rs->state = RS_FREE;
        rs->rob_slot = nullptr;
        rs->input_opt_ready_cnt = 0;
        free_stack[free_stack_size] = rs->rs_index;
        free_stack_size++;
    }

};

#endif //CPU_EMULATOR_BACKEND_H
