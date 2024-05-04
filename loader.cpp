//
// Created by pveentjer on 4/30/24.
//

#include "loader.h"
#include "instructions.h"
#include "cpu.h"
#include <fstream>
#include <sstream>

using namespace std;

bool isValidLabel(const string &s)
{
    for (char c: s)
    {
        if (!isalpha(c))
        {
            return false;
        }
    }
    return true;
}

bool isValidVariable(const string &s)
{
    for (char c: s)
    {
        if (!isalpha(c))
        {
            return false;
        }
    }
    return true;
}

void load_program(CPU *cpu, string file)
{
    ifstream infile(file);
    if (!infile.is_open())
    {
        cerr << "Failed to open program file." << endl;
        throw std::runtime_error("Failed to open program file.");
    }

    unordered_map<string, int> labels = unordered_map<string, int>();
    unordered_map<string, int> variables = unordered_map<string, int>();
    string lineText;
    uint32_t line_nr = 0;
    int heapLimit = 0;
    while (getline(infile, lineText))
    {
        line_nr++;

        size_t commentPos = lineText.find(';');
        if (commentPos != string::npos)
        {
            // Remove the comment part
            lineText = lineText.substr(0, commentPos);
        }

        lineText = trim(lineText);

        if (lineText.empty())
        {
            continue;
        }

        istringstream iss(lineText);

        string first_token;
        if (!(iss >> first_token))
        {
            cerr << "Invalid instruction format at line " << line_nr << "." << endl;
            throw std::runtime_error("Invalid program");
        }

        // check if it is a variable
        if (first_token == "VAR")
        {
            string name;
            int value;
            if (iss >> name >> value)
            {
                if (!isValidVariable(name))
                {
                    cerr << "Invalid VAR name [" << name << "] at line_nr " << line_nr << "." << endl;
                    throw std::runtime_error("Invalid program");
                }

                optional<int> existing_variable = mapGet(variables, name);
                if (existing_variable.has_value())
                {
                    cerr << "Variable [" << name << "] not unique at line " << line_nr << "." << endl;
                    throw std::runtime_error("Invalid program");
                }

                int address = heapLimit;
                cpu->memory->at(address) = value;
                variables.insert({name, address});
                heapLimit++;
            }
            else
            {
                cerr << "Invalid VAR format at line_nr " << line_nr << "." << endl;
                throw std::runtime_error("Invalid program");
            }
            continue;
        }

        // check if it is a label
        if (endsWith(first_token, ":"))
        {
            string label_name = removeLast(first_token);
            if (!isValidLabel(label_name))
            {
                cerr << "Invalid label_name [" << label_name << "]at line " << line_nr << "." << endl;
                throw std::runtime_error("Invalid program");
            }

            optional<int> existing_label = mapGet(labels, label_name);
            if (existing_label.has_value())
            {
                cerr << "Label [" << label_name << "] not unique at line " << line_nr << "." << endl;
                throw std::runtime_error("Invalid program");
            }

            labels.insert({label_name, cpu->frontend.code->size()});
            continue;
        }

        Instr instr;
        auto it = MNEMONIC_TO_OPCODE.find(first_token);
        if (it != MNEMONIC_TO_OPCODE.end())
        {
            instr.opcode = it->second;

            switch (instr.opcode)
            {
                case OPCODE_AND:
                {
                    int r_src1, r_src2, r_dst;
                    if (iss >> r_dst >> r_src1 >> r_src2)
                    {
                        instr.input_ops_cnt = 2;
                        instr.input_ops[0].type = OperandType::REGISTER;
                        instr.input_ops[0].reg = r_src1;
                        instr.input_ops[1].type = OperandType::REGISTER;
                        instr.input_ops[1].reg = r_src2;

                        instr.output_ops_cnt = 1;
                        instr.output_ops[0].type = OperandType::REGISTER;
                        instr.output_ops[0].reg = r_dst;
                    }
                    else
                    {
                        cerr << "Invalid AND instruction format at line_nr " << line_nr << "." << endl;
                        throw std::runtime_error("Invalid AND");
                    }
                    break;
                }
                case OPCODE_OR:
                {
                    int r_src1, r_src2, r_dst;
                    if (iss >> r_dst >> r_src1 >> r_src2)
                    {
                        instr.input_ops_cnt = 2;
                        instr.input_ops[0].type = OperandType::REGISTER;
                        instr.input_ops[0].reg = r_src1;
                        instr.input_ops[1].type = OperandType::REGISTER;
                        instr.input_ops[1].reg = r_src2;

                        instr.output_ops_cnt = 1;
                        instr.output_ops[0].type = OperandType::REGISTER;
                        instr.output_ops[0].reg = r_dst;
                    }
                    else
                    {
                        cerr << "Invalid OR instruction format at line_nr " << line_nr << "." << endl;
                        throw std::runtime_error("Invalid OR");
                    }
                    break;
                }
                case OPCODE_XOR:
                {
                    int r_src1, r_src2, r_dst;
                    if (iss >> r_dst >> r_src1 >> r_src2)
                    {
                        instr.input_ops_cnt = 2;
                        instr.input_ops[0].type = OperandType::REGISTER;
                        instr.input_ops[0].reg = r_src1;
                        instr.input_ops[1].type = OperandType::REGISTER;
                        instr.input_ops[1].reg = r_src2;

                        instr.output_ops_cnt = 1;
                        instr.output_ops[0].type = OperandType::REGISTER;
                        instr.output_ops[0].reg = r_dst;
                    }
                    else
                    {
                        cerr << "Invalid XOR instruction format at line_nr " << line_nr << "." << endl;
                        throw std::runtime_error("Invalid OR");
                    }
                    break;
                }
                case OPCODE_NOT:
                {
                    int r_src, r_dst;
                    if (iss >> r_dst >> r_src)
                    {
                        instr.input_ops_cnt = 1;
                        instr.input_ops[0].type = OperandType::REGISTER;
                        instr.input_ops[0].reg = r_src;

                        instr.output_ops_cnt = 1;
                        instr.output_ops[0].type = OperandType::REGISTER;
                        instr.output_ops[0].reg = r_src;
                    }
                    else
                    {
                        cerr << "Invalid NOT instruction format at line_nr " << line_nr << "." << endl;
                        throw std::runtime_error("Invalid NOT");
                    }
                    break;
                }
                case OPCODE_ADD:
                {
                    int r_src1, r_src2, r_dst;
                    if (iss >> r_dst >> r_src1 >> r_src2)
                    {
                        instr.input_ops_cnt = 2;
                        instr.input_ops[0].type = OperandType::REGISTER;
                        instr.input_ops[0].reg = r_src1;
                        instr.input_ops[1].type = OperandType::REGISTER;
                        instr.input_ops[1].reg = r_src2;

                        instr.output_ops_cnt = 1;
                        instr.output_ops[0].type = OperandType::REGISTER;
                        instr.output_ops[0].reg = r_dst;
                    }
                    else
                    {
                        cerr << "Invalid ADD instruction format at line_nr " << line_nr << "." << endl;
                        throw std::runtime_error("Invalid ADD");
                    }
                    break;
                }
                case OPCODE_SUB:
                {
                    int r_src1, r_src2, r_dst;
                    if (iss >> r_dst >> r_src1 >> r_src2)
                    {
                        instr.input_ops_cnt = 2;
                        instr.input_ops[0].type = OperandType::REGISTER;
                        instr.input_ops[0].reg = r_src1;
                        instr.input_ops[1].type = OperandType::REGISTER;
                        instr.input_ops[1].reg = r_src2;

                        instr.output_ops_cnt = 1;
                        instr.output_ops[0].type = OperandType::REGISTER;
                        instr.output_ops[0].reg = r_dst;
                    }
                    else
                    {
                        cerr << "Invalid SUB instruction format at line_nr " << line_nr << "." << endl;
                        throw std::runtime_error("Invalid SUB");
                    }
                    break;
                }
                case OPCODE_DEC:
                {
                    int r_src;
                    if (iss >> r_src)
                    {
                        instr.input_ops_cnt = 1;
                        instr.input_ops[0].type = OperandType::REGISTER;
                        instr.input_ops[0].reg = r_src;

                        instr.output_ops_cnt = 1;
                        instr.output_ops[0].type = OperandType::REGISTER;
                        instr.output_ops[0].reg = r_src;
                    }
                    else
                    {
                        cerr << "Invalid DEC instruction format at line_nr " << line_nr << "." << endl;
                        throw std::runtime_error("Invalid DEC");
                    }
                    break;
                }
                case OPCODE_INC:
                {
                    int r_src;
                    if (iss >> r_src)
                    {
                        instr.input_ops_cnt = 1;
                        instr.input_ops[0].type = OperandType::REGISTER;
                        instr.input_ops[0].reg = r_src;

                        instr.output_ops_cnt = 1;
                        instr.output_ops[0].type = OperandType::REGISTER;
                        instr.output_ops[0].reg = r_src;
                    }
                    else
                    {
                        cerr << "Invalid INC instruction format at line_nr " << line_nr << "." << endl;
                        throw std::runtime_error("Invalid INC");
                    }
                    break;
                }
                case OPCODE_LOAD:
                {
                    string name;
                    int r_dst;
                    if (iss >> r_dst >> name)
                    {
                        optional<int> variable = mapGet(variables, name);
                        if (!variable.has_value())
                        {
                            cerr << "Variable [" << name << "] not found at line " << line_nr << "." << endl;
                            throw std::runtime_error("Invalid program");
                        }

                        instr.input_ops_cnt = 1;
                        instr.input_ops[0].type = OperandType::MEMORY;
                        instr.input_ops[0].memory_addr = variable.value();

                        instr.output_ops_cnt = 1;
                        instr.output_ops[0].type = OperandType::REGISTER;
                        instr.output_ops[0].reg = r_dst;
                    }
                    else
                    {
                        cerr << "Invalid LOAD instruction format at line_nr " << line_nr << "." << endl;
                        throw std::runtime_error("Invalid LOAD");
                    }
                    break;
                }
                case OPCODE_STORE:
                {
                    string name;
                    int r_src;
                    if (iss >> r_src >> name)
                    {
                        optional<int> variable = mapGet(variables, name);
                        if (!variable.has_value())
                        {
                            cerr << "Variable [" << name << "] not found at line " << line_nr << "." << endl;
                            throw std::runtime_error("Invalid STORE");
                        }

                        instr.input_ops_cnt = 1;
                        instr.input_ops[0].type = OperandType::REGISTER;
                        instr.input_ops[0].reg = r_src;

                        instr.output_ops_cnt = 1;
                        instr.output_ops[0].type = OperandType::MEMORY;
                        instr.output_ops[0].memory_addr = variable.value();
                    }
                    else
                    {
                        cerr << "Invalid STORE instruction format at line_nr " << line_nr << "." << endl;
                        throw std::runtime_error("Invalid STORE");
                    }
                    break;
                }
                case OPCODE_JNZ:
                {
                    int r_src;
                    string label;
                    if (iss >> r_src >> label)
                    {
                        optional<int> labelAddr = mapGet(labels, label);
                        if (labelAddr.has_value())
                        {
                            instr.input_ops_cnt = 2;
                            instr.input_ops[0].type = OperandType::REGISTER;
                            instr.input_ops[0].reg = r_src;
                            instr.input_ops[1].type = OperandType::CODE;
                            instr.input_ops[1].code_addr = labelAddr.value();
                        }
                        else
                        {
                            cerr << "Unknown target [" << label << "] at line_nr " << line_nr << "."
                                 << endl;
                            throw std::runtime_error("Invalid JNZ");
                        }
                    }
                    else
                    {
                        cerr << "Invalid JNZ instruction format at line_nr " << line_nr << "." << endl;
                        throw std::runtime_error("Invalid JNZ");
                    }
                    break;
                }
                case OPCODE_PRINTR:
                {


                    int r_src;
                    if (iss >> r_src)
                    {
                        instr.input_ops_cnt = 1;
                        instr.input_ops[0].type = OperandType::REGISTER;
                        instr.input_ops[0].reg = r_src;

                        instr.output_ops_cnt = 0;
                    }
                    else
                    {
                        cerr << "Invalid PRINTR instruction format at line_nr " << line_nr << "." << endl;
                        throw std::runtime_error("Invalid PRINTR");
                    }
                    break;
                }
                case OPCODE_HALT:
                    instr.input_ops_cnt = 0;
                    instr.output_ops_cnt = 0;
                    break;
                case OPCODE_NOP:
                    instr.input_ops_cnt = 0;
                    instr.output_ops_cnt = 0;
                    break;

                    // todo: not all opcodes are handled.
            }

            cpu->frontend.code->push_back(instr);
        }
        else
        {
            cerr << "Unknown mnemonic: " << first_token << endl;
            throw std::runtime_error("Invalid program");
        }
    }

    // todo: file is not closed on error.
    infile.close();
    if (cpu->frontend.code->size() > 0)
    {
        cpu->frontend.ip_next_fetch = 0;
    }
    else
    {
        cpu->frontend.ip_next_fetch = -1;
    }
}
