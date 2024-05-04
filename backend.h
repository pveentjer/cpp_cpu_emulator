//
// Created by pveentjer on 5/3/24.
//

#ifndef CPU_EMULATOR_BACKEND_H
#define CPU_EMULATOR_BACKEND_H


#include "memory_subsystem.h"
#include "frontend.h"

enum ROB_Slot_State {
    ROB_SLOT_NEW,
    ROB_SLOT_EXECUTED
};

struct ROB_Slot
{
    Instr *instr;
    int result;
    ROB_Slot_State state;
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

struct RS;
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


/**
 * The Backend is responsible for the actual execution of the instruction.
 *
 * It will take instructions from the InstrQueue.
 */
struct Backend
{
    RS *rs_array;

    // A stack of free RS.
    uint16_t *rs_free_stack;
    uint16_t rs_free_stack_size;

    // A circular queue with RS that are ready to be submitted
    uint16_t *rs_ready_queue;
    uint64_t rs_ready_tail, rs_ready_head;
    // the register alias table used for register renaming
    unordered_map<uint16_t ,uint16_t> *rat;

    Frontend *frontend;
    // when true, prints every instruction before being executed.
    bool trace;
    int *arch_regs;
    int *phys_regs;
    StoreBuffer *sb;
    vector<int> *memory;
    InstrQueue *instr_queue;
    ROB rob;
    ExecutionUnit eu;

    uint16_t rs_count;

    void cycle();

    bool is_idle();

    void on_rs_ready(RS *rs);

    void retire(ROB_Slot *rob_slot);

    void init_rs(RS *rs, ROB_Slot *rob_slot);

    void cycle_retire();

    void cycle_dispatch();

    void cycle_issue();
};

enum RS_State{
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

    // the indices of the physical registers.
    int src_phys_registers[MAX_INPUT_OPERANDS];

    int dst_phys_reg = -1;

    // number of src_phys_registers in_operands required
    int input_op_cnt;

    // number of src_phys_registers in_operands available
    int src_completed_cnt;

    ROB_Slot *rob_slot;

    Backend *backend;

    int *phys_regs;
};
#endif //CPU_EMULATOR_BACKEND_H
