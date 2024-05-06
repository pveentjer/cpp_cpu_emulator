#include "cpu.h"
#include "loader.h"

int main()
{
    CPU_Config *config = new CPU_Config();
    config->trace = true;
    config->cpu_frequency_Hz = 10;
    config->memory_size_ints = 16;
    config->arch_reg_cnt = 16;
    config->phys_reg_cnt = 64;
    config->sb_capacity = 4;
    config->rs_count = 16;
    config->frontend_n_wide = 2;
    config->instr_queue_capacity = 64;
    config->rob_capacity = 32;
    config->debug = true;

    CPU *cpu = new CPU(*config);

    load_program(cpu, "program2.asm");

    cpu->run();
    cpu->print_memory();
    return 0;
}
