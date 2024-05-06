//
// Created by pveentjer on 4/30/24.
//

#include "../include/cpu.h"

bool CPU::is_idle()
{
    return frontend->is_idle()
           && instr_queue->is_empty()
           //&& backend.is_empty()
           && sb->is_empty();
}

void CPU::cycle()
{
    // todo: reverse order

    frontend->cycle();

    backend->cycle();

    sb->cycle();

    cycles++;
}

void CPU::run()
{
    while (!is_idle())
    {
        this_thread::sleep_for(cycle_period_ms);

        if (debug)
        {
            printf("---------------------------------\n");
            printf("cycle: %lu\n", cycles);
            printf("---------------------------------\n");
        }

        cycle();
    }
}

void CPU::print_memory() const
{
    printf("------------------Memory----------------\n");
    for (int k = 0; k < memory->size(); k++)
    {
        printf("%04d %04d\n", k, memory->at(k));
    }
}
