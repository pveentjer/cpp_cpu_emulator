#include <optional>
#include "cpu.h"
#include "loader.h"

int main()
{
    CPU *cpu = new CPU();
    cpu->trace = true;
    cpu->cpuFrequencyHz=10;
    int res = load_program(cpu, "program.txt");
    if (res != 0)
    {
        return -1;
    }

    while (!cpu->is_idle())
    {
        cpu->tick();
    }

    cpu->print_memory();
    return 0;
}
