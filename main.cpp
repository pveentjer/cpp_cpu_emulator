#include "cpu.h"
#include "loader.h"

int main()
{
    CPU_Config *config = new CPU_Config();
    config->trace = true;
    config->cpu_frequency_Hz=10;

    CPU *cpu = new CPU(*config);

    int res = load_program(cpu, "program.asm");
    if (res != 0)
    {
        return -1;
    }

    cpu->run();
    cpu->print_memory();
    return 0;
}
