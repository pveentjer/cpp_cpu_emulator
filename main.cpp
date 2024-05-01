#include "cpu.h"
#include "loader.h"

int main()
{
    CPU *cpu = new CPU();
    cpu->setTrace(true);
    cpu->setCpuFrequencyHz(10);

    int res = load_program(cpu, "program.asm");
    if (res != 0)
    {
        return -1;
    }

    cpu->run();
    cpu->print_memory();
    return 0;
}
