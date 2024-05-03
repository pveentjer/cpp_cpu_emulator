//
// Created by pveentjer on 5/3/24.
//

#ifndef CPU_EMULATOR_BACKEND_H
#define CPU_EMULATOR_BACKEND_H


#include "memory_subsystem.h"
#include "frontend.h"

static const int SLOT_NEW = 0;
static const int SLOT_EXECUTED = 0;

struct ROB_Slot
{
    Instr *instr;
    int result;
    int state;
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

    bool is_empty()
    {
        return head == tail;
    }

    uint16_t size()
    {
        return tail - head;
    }

    bool is_full()
    {
        return size() == capacity;
    }
};

struct RS;
struct Backend;


struct ExecutionUnit
{
    RS *rs;
    StoreBuffer *sb;
    Backend *backend;
    vector<int> *arch_regs;
    vector<int> *memory;
    uint16_t rs_target;
    RS *rs_array;
    uint8_t rs_src_index;

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
    uint16_t *rs_free_array;
    uint16_t rs_free_count;

    uint16_t *rs_ready_array;
    uint64_t rs_ready_tail, rs_ready_head;

    Frontend *frontend;
    // when true, prints every instruction before being executed.
    bool trace;
    vector<int> *arch_regs;
    StoreBuffer *sb;
    vector<int> *memory;
    InstrQueue *instr_queue;
    ROB rob;
    ExecutionUnit eu;

    uint16_t rs_count;

    void cycle();

    bool is_idle();

    void broadcast_rs_ready(RS *rs);

    void init_rs(RS *rs, ROB_Slot *rob_slot);
};

enum RS_State{
    // The RS is waiting for 1 or more of its src operands.
    RS_WAITING,
    // The RS has all operands ready, but not yet submitted
    RS_READY,
    // not used yet; but as soon as we set a queue between the RS and EU, this is needed
    RS_SUBMITTED,
    // The instruction has executed.
    RS_EXECUTED,
};

// todo: there could be multiple dependend rs waiting for the result of this RS
// A reservation station
struct RS
{
    RS_State state;

    uint16_t rs_index;
    // The maximum num
    int src[MAX_SRC_OPERANDS];

    // todo: doesn't work because of multiple target rs.
    // the destination RS that should get the result
    uint16_t dst_rs;
    // the index of the src in the dst RS
    uint16_t dst_src_index;

    int result;
    // number of src operands required
    int src_required_cnt;
    // number of src operands available
    int src_completed_cnt;
    ROB_Slot *rob_slot;
    Backend *backend;

    // todo: what happens when the rs is the last free one and
    // and it reserved for an instruction, where should it send
    // its src?
    //void srcReady(uint16_t src_index, int src);

    //void storeResult();
};
#endif //CPU_EMULATOR_BACKEND_H
