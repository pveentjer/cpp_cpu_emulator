#include <iostream>
#include <vector>
#include <thread>
#include <optional>
#include <map>
#include <fstream>
#include <algorithm>
#include <sstream>
#include "instructions.h"
#include "utils.h"
#include "cpu.h"
#include "loader.h"

int main()
{
    CPU *cpu = new CPU();
    cpu->trace = true;

    int res = load_program(cpu, "program.txt");
    if (res != 0)
    {
        return -1;
    }

    while (!cpu->tick_again())
    {
        cpu->tick();
    }

    cpu->print_memory();
    return 0;
}
