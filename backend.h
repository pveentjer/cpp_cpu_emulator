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


struct Phys_Reg
{
    uint16_t id;
    int value;
    bool valid;
};

/**
 * The Backend is responsible for the actual execution of the instruction.
 *
 * It will take instructions from the InstrQueue.
 */
struct Backend
{
    // the array with the reservation stations
    RS *rs_array;
    // the number of reservation stations
    uint16_t rs_count;

    // A stack of free RS.
    uint16_t *rs_free_stack;
    uint16_t rs_free_stack_size;


    // A circular queue with RS that are ready to be submitted
    uint16_t *rs_ready_queue;
    uint64_t rs_ready_tail, rs_ready_head;

    Frontend *frontend;
    // when true, prints every instruction before being executed.
    bool trace;
    int *arch_regs;
    StoreBuffer *sb;
    vector<int> *memory;
    InstrQueue *instr_queue;
    ROB rob;
    ExecutionUnit eu;
    RAT *rat;

    Phys_Reg *phys_reg_array;
    int *phys_reg_free_stack;
    uint16_t phys_reg_free_stack_size;


    void cycle();

    bool is_idle();

    void on_rs_ready(RS *rs);

    void retire(ROB_Slot *rob_slot);

    void init_rs(RS *rs, ROB_Slot *rob_slot);

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

// todo: there could be multiple dependend rs waiting for the result of this RS
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
#endif //CPU_EMULATOR_BACKEND_H
