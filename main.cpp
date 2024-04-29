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

static const int REGISTER_COUNT = 32;
static const int MEMORY_SIZE = 16;
static const int STORE_BUFFER_CAPACITY = 4;
static const int CPU_FREQUENCY_HZ = 1;

struct StoreBufferEntry {
    int value;
    int addr;
};

struct StoreBuffer {
    StoreBufferEntry entries[STORE_BUFFER_CAPACITY];
    uint64_t head;
    uint64_t tail;

    std::optional<int> lookup(int addr) {
        // todo: instead of iterating over all values, there should be a directly-mapped hash-table
        // so that we can use the last 12 bits of the address and do a lookup. Then we also need
        // to handle the 4K aliasing problem.
        for (uint64_t k = tail; k < head; k++) {
            StoreBufferEntry &entry = entries[k % STORE_BUFFER_CAPACITY];
            if (entry.addr == addr) {
                return std::optional<int>(entry.value);
            }
        }

        return std::nullopt;
    }

    void write(int addr, int value) {
        StoreBufferEntry &entry = entries[tail % STORE_BUFFER_CAPACITY];
        entry.value = value;
        entry.addr = addr;
        tail++;
    }

    void tick(std::vector<int> *memory) {
        if (head != tail) {
            StoreBufferEntry &entry = entries[head % STORE_BUFFER_CAPACITY];
            memory->at(entry.addr) = entry.value;
            head++;
        }
    }
};


bool isValidLabel(const std::string &s) {
    for (char c: s) {
        if (!std::isalpha(c)) {
            return false;
        }
    }
    return true;
}

class CPU {

public:
    int32_t ip;
    std::vector<Instruction> *program;
    std::vector<int> *registers;
    std::vector<int> *memory;
    StoreBuffer sb;
    // when true, prints every instruction before being executed.
    bool trace;

    CPU() {
        ip = 0;
        program = new std::vector<Instruction>();
        registers = new std::vector<int>();
        for (int k = 0; k < REGISTER_COUNT; k++) {
            registers->push_back(0);
        }
        memory = new std::vector<int>();
        for (int k = 0; k < MEMORY_SIZE; k++) {
            memory->push_back(0);
        }
        sb.head = 0;
        sb.tail = 0;
        trace = false;
    }

    int load_program(std::string file) {
        // Open the file containing the program instructions
        std::ifstream infile(file);
        if (!infile.is_open()) {
            std::cerr << "Failed to open program file." << std::endl;
            return 1;
        }

        std::unordered_map<std::string, int> labels = std::unordered_map<std::string, int>();
        std::string lineText;
        uint32_t line_nr = 0;
        while (std::getline(infile, lineText)) {
            line_nr++;

            size_t commentPos = lineText.find(';');
            if (commentPos != std::string::npos) {
                // Remove the comment part
                lineText = lineText.substr(0, commentPos);
            }

            lineText = trim(lineText);

            if (lineText.empty()) {
                continue;
            }

            printf("[%s]\n", lineText.c_str());

            std::istringstream iss(lineText);

            std::string first_token;
            if (!(iss >> first_token)) {
                std::cerr << "Invalid instruction format at line " << line_nr << "." << std::endl;
                return 1;
            }

            // check if it is a label
            if (endsWith(first_token, ":")) {
                std::string label = removeLast(first_token);
                if (!isValidLabel(label)) {
                    std::cerr << "Invalid label [" << label << "]at line " << line_nr << "." << std::endl;
                    return 1;

                }

                std::optional<int> x = mapGet(labels, label);
                if (x.has_value()) {
                    std::cerr << "Label [" << label << "] not unique at line " << line_nr << "." << std::endl;
                    return 1;
                }

                labels.insert({label, program->size()});
                continue;
            }

            Instruction instr;
            auto it = MNEMONIC_TO_OPCODE.find(first_token);
            if (it != MNEMONIC_TO_OPCODE.end()) {
                instr.opcode = it->second;

                switch (instr.opcode) {
                    case OPCODE_AND: {
                        int r_src1, r_src2;
                        if (iss >> r_src1 >> r_src2) {
                            instr.code.AND.r_src1 = r_src1;
                            instr.code.AND.r_src2 = r_src2;
                        } else {
                            std::cerr << "Invalid AND instruction format at line_nr " << line_nr << "." << std::endl;
                            return 1;
                        }
                        break;
                    }
                    case OPCODE_OR: {
                        int r_src1, r_src2;
                        if (iss >> r_src1 >> r_src2) {
                            instr.code.OR.r_src1 = r_src1;
                            instr.code.OR.r_src2 = r_src2;
                        } else {
                            std::cerr << "Invalid OR instruction format at line_nr " << line_nr << "." << std::endl;
                            return 1;
                        }
                        break;
                    }
                    case OPCODE_NOT: {
                        int r_src;
                        if (iss >> r_src) {
                            instr.code.NOT.r_src = r_src;
                        } else {
                            std::cerr << "Invalid NOT instruction format at line_nr " << line_nr << "." << std::endl;
                            return 1;
                        }
                        break;
                    }
                    case OPCODE_ADD: {
                        int r_src1, r_src2;
                        if (iss >> r_src1 >> r_src2) {
                            instr.code.ADD.r_src1 = r_src1;
                            instr.code.ADD.r_src2 = r_src2;
                        } else {
                            std::cerr << "Invalid ADD instruction format at line_nr " << line_nr << "." << std::endl;
                            return 1;
                        }
                        break;
                    }
                    case OPCODE_SUB: {
                        int r_src1, r_src2;
                        if (iss >> r_src1 >> r_src2) {
                            instr.code.SUB.r_src1 = r_src1;
                            instr.code.SUB.r_src2 = r_src2;
                        } else {
                            std::cerr << "Invalid SUB instruction format at line_nr " << line_nr << "." << std::endl;
                            return 1;
                        }
                        break;
                    }
                    case OPCODE_DEC: {
                        int r_src;
                        if (iss >> r_src) {
                            instr.code.DEC.r_src = r_src;
                        } else {
                            std::cerr << "Invalid DEC instruction format at line_nr " << line_nr << "." << std::endl;
                            return 1;
                        }
                        break;
                    }
                    case OPCODE_INC: {
                        int r_src;
                        if (iss >> r_src) {
                            instr.code.INC.r_src = r_src;
                        } else {
                            std::cerr << "Invalid INC instruction format at line_nr " << line_nr << "." << std::endl;
                            return 1;
                        }
                        break;
                    }
                    case OPCODE_LOAD: {
                        int m_src, r_dst;
                        if (iss >> m_src >> r_dst) {
                            instr.code.LOAD.m_src = m_src;
                            instr.code.LOAD.r_dst = r_dst;
                        } else {
                            std::cerr << "Invalid LOAD instruction format at line_nr " << line_nr << "." << std::endl;
                            return 1;
                        }
                        break;
                    }
                    case OPCODE_STORE: {
                        int r_src, m_dst;
                        if (iss >> r_src >> m_dst) {
                            instr.code.STORE.r_src = r_src;
                            instr.code.STORE.m_dst = m_dst;
                        } else {
                            std::cerr << "Invalid STORE instruction format at line_nr " << line_nr << "." << std::endl;
                            return 1;
                        }
                        break;
                    }
                    case OPCODE_JNZ: {
                        int r_src;
                        std::string label;
                        if (iss >> r_src >> label) {
                            instr.code.JNZ.r_src = r_src;
                            std::optional<int> labelAddr = mapGet(labels, label);
                            if (labelAddr.has_value()) {
                                instr.code.JNZ.p_target = labelAddr.value();
                            } else {
                                std::cerr << "Unknown target [" << label << "] at line_nr " << line_nr << "."
                                          << std::endl;
                                return 1;
                            }
                        } else {
                            std::cerr << "Invalid JNZ instruction format at line_nr " << line_nr << "." << std::endl;
                            return 1;
                        }
                        break;
                    }
                    case OPCODE_PRINTR: {
                        int r_src;
                        if (iss >> r_src) {
                            instr.code.PRINTR.r_src = r_src;
                        } else {
                            std::cerr << "Invalid PRINTR instruction format at line_nr " << line_nr << "." << std::endl;
                            return 1;
                        }
                        break;
                    }
                }

                program->push_back(instr);
            } else {
                std::cerr << "Unknown mnemonic: " << first_token << std::endl;
                return 1;
            }
        }

        // todo: file is not closed on error.
        // Close the file
        infile.close();
        return 0;
    }

    void print_memory() {
        printf("------------------Memory----------------\n");
        for (int k = 0; k < memory->size(); k++) {
            printf("%04d %04d\n", k, memory->at(k));
        }
    }

    void execute(Instruction *instr) {
        switch (instr->opcode) {
            case OPCODE_ADD: {
                int v1 = registers->at(instr->code.ADD.r_src1);
                int v2 = registers->at(instr->code.ADD.r_src2);
                registers->at(instr->code.ADD.r_dst) = v1 + v2;
                ip++;
                break;
            }
            case OPCODE_SUB: {
                int v1 = registers->at(instr->code.SUB.r_src1);
                int v2 = registers->at(instr->code.SUB.r_src2);
                registers->at(instr->code.SUB.r_dst) = v1 + v2;
                ip++;
                break;
            }
            case OPCODE_AND: {
                int v1 = registers->at(instr->code.AND.r_src1);
                int v2 = registers->at(instr->code.AND.r_src2);
                registers->at(instr->code.AND.r_dst) = v1 && v2;
                ip++;
                break;
            }
            case OPCODE_OR: {
                int v1 = registers->at(instr->code.OR.r_src1);
                int v2 = registers->at(instr->code.OR.r_src2);
                registers->at(instr->code.OR.r_dst) = v1 || v2;
                ip++;
                break;
            }
            case OPCODE_NOT: {
                int v1 = registers->at(instr->code.NOT.r_src);
                registers->at(instr->code.OR.r_dst) = !v1;
                ip++;
                break;
            }
            case OPCODE_CMP: {
                int v1 = registers->at(instr->code.CMP.r_src1);
                int v2 = registers->at(instr->code.CMP.r_src2);
                registers->at(instr->code.CMP.r_dst) = v1 == v2;
                ip++;
                break;
            }
            case OPCODE_INC: {
                registers->at(instr->code.INC.r_src)++;
                ip++;
                break;
            }
            case OPCODE_DEC: {
                registers->at(instr->code.DEC.r_src)--;
                ip++;
                break;
            }
            case OPCODE_MOV: {
                registers->at(instr->code.MOV.r_dst) = registers->at(instr->code.MOV.r_src);
                ip++;
                break;
            }
            case OPCODE_LOAD: {
                // a primitive version of store to load forwarding. Because of the store buffer
                // we first need to look there before returning the value otherwise the CPU would
                // not be able to see some of its own writes and become incoherent.

                int value = sb.lookup(instr->code.LOAD.m_src)
                        .value_or(memory->at(instr->code.LOAD.m_src));

                registers->at(instr->code.LOAD.r_dst) = value;
                ip++;
                break;
            }
            case OPCODE_STORE: {
                sb.write(instr->code.STORE.m_dst, registers->at(instr->code.STORE.r_src));
                ip++;
                break;
            }
            case OPCODE_PRINTR: {
                int v1 = registers->at(instr->code.PRINTR.r_src);
                printf("R%d=%d\n", instr->code.PRINTR.r_src, v1);
                ip++;
                break;
            }
            case OPCODE_JNZ: {
                int v1 = registers->at(instr->code.JNZ.r_src);
                if (v1 != 0) {
                    ip = instr->code.JNZ.p_target;
                } else {
                    ip++;
                }
                break;
            }
            case OPCODE_HALT: {
                ip = -1;
                break;
            }
            default:
                throw std::runtime_error("Unrecognized opcode");
        }
    }

    bool tick_again() const {
        if (ip > -1) {
            return false;
        }
        return sb.head == sb.tail;
    }

    void tick() {
        double pause = 1.0 / CPU_FREQUENCY_HZ;
        std::chrono::milliseconds period(static_cast<int>(pause * 1000));
        std::this_thread::sleep_for(period);

        if (ip > -1) {
            Instruction *instr = &program->at(ip);

            if (trace) {
                print_instr(instr);
            }

            execute(instr);
        }

        sb.tick(memory);
    }
};

int main() {
    CPU *cpu = new CPU();
    cpu->trace = true;
    cpu->memory->at(0) = 5;

    int res = cpu->load_program("program.txt");
    if (res != 0) {
        return -1;
    }

    while (!cpu->tick_again()) {
        cpu->tick();
    }

    cpu->print_memory();
    return 0;
}
