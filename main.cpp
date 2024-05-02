#include "cpu.h"
#include "loader.h"

int main()
{
    CPU_Config *config = new CPU_Config();
    config->trace = true;
    config->cpu_frequency_Hz=10;
    config->memory_size = 16;
    config->arch_reg_count = 16;
    config->sb_capacity = 4;

    CPU *cpu = new CPU(*config);

    load_program(cpu, "program.asm");

    cpu->run();
    cpu->print_memory();
    return 0;
}
