//
// Created by pveentjer on 4/30/24.
//

#include "loader.h"
#include "instructions.h"
#include "cpu.h"

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


int load_program(CPU *cpu, string file)
{
    // Open the file containing the program instructions
    ifstream infile(file);
    if (!infile.is_open())
    {
        cerr << "Failed to open program file." << endl;
        return 1;
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
            return 1;
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
                    return 1;
                }

                optional<int> existing_variable = mapGet(variables, name);
                if (existing_variable.has_value())
                {
                    cerr << "Variable [" << name << "] not unique at line " << line_nr << "." << endl;
                    return 1;
                }

                int address = heapLimit;
                cpu->memory->at(address) = value;
                variables.insert({name, address});
                heapLimit++;
            }
            else
            {
                cerr << "Invalid VAR format at line_nr " << line_nr << "." << endl;
                return 1;
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
                return 1;

            }

            optional<int> existing_label = mapGet(labels, label_name);
            if (existing_label.has_value())
            {
                cerr << "Label [" << label_name << "] not unique at line " << line_nr << "." << endl;
                return 1;
            }

            labels.insert({label_name, cpu->program->size()});
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
                    int r_src1, r_src2;
                    if (iss >> r_src1 >> r_src2)
                    {
                        instr.code.AND.r_src1 = r_src1;
                        instr.code.AND.r_src2 = r_src2;
                    }
                    else
                    {
                        cerr << "Invalid AND instruction format at line_nr " << line_nr << "." << endl;
                        return 1;
                    }
                    break;
                }
                case OPCODE_OR:
                {
                    int r_src1, r_src2;
                    if (iss >> r_src1 >> r_src2)
                    {
                        instr.code.OR.r_src1 = r_src1;
                        instr.code.OR.r_src2 = r_src2;
                    }
                    else
                    {
                        cerr << "Invalid OR instruction format at line_nr " << line_nr << "." << endl;
                        return 1;
                    }
                    break;
                }
                case OPCODE_NOT:
                {
                    int r_src;
                    if (iss >> r_src)
                    {
                        instr.code.NOT.r_src = r_src;
                    }
                    else
                    {
                        cerr << "Invalid NOT instruction format at line_nr " << line_nr << "." << endl;
                        return 1;
                    }
                    break;
                }
                case OPCODE_ADD:
                {
                    int r_src1, r_src2;
                    if (iss >> r_src1 >> r_src2)
                    {
                        instr.code.ADD.r_src1 = r_src1;
                        instr.code.ADD.r_src2 = r_src2;
                    }
                    else
                    {
                        cerr << "Invalid ADD instruction format at line_nr " << line_nr << "." << endl;
                        return 1;
                    }
                    break;
                }
                case OPCODE_SUB:
                {
                    int r_src1, r_src2;
                    if (iss >> r_src1 >> r_src2)
                    {
                        instr.code.SUB.r_src1 = r_src1;
                        instr.code.SUB.r_src2 = r_src2;
                    }
                    else
                    {
                        cerr << "Invalid SUB instruction format at line_nr " << line_nr << "." << endl;
                        return 1;
                    }
                    break;
                }
                case OPCODE_DEC:
                {
                    int r_src;
                    if (iss >> r_src)
                    {
                        instr.code.DEC.r_src = r_src;
                    }
                    else
                    {
                        cerr << "Invalid DEC instruction format at line_nr " << line_nr << "." << endl;
                        return 1;
                    }
                    break;
                }
                case OPCODE_INC:
                {
                    int r_src;
                    if (iss >> r_src)
                    {
                        instr.code.INC.r_src = r_src;
                    }
                    else
                    {
                        cerr << "Invalid INC instruction format at line_nr " << line_nr << "." << endl;
                        return 1;
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
                            return 1;
                        }

                        instr.code.LOAD.m_src = variable.value();
                        instr.code.LOAD.r_dst = r_dst;
                    }
                    else
                    {
                        cerr << "Invalid LOAD instruction format at line_nr " << line_nr << "." << endl;
                        return 1;
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
                            return 1;
                        }

                        instr.code.STORE.r_src = r_src;
                        instr.code.STORE.m_dst = variable.value();
                    }
                    else
                    {
                        cerr << "Invalid STORE instruction format at line_nr " << line_nr << "." << endl;
                        return 1;
                    }
                    break;
                }
                case OPCODE_JNZ:
                {
                    int r_src;
                    string label;
                    if (iss >> r_src >> label)
                    {
                        instr.code.JNZ.r_src = r_src;
                        optional<int> labelAddr = mapGet(labels, label);
                        if (labelAddr.has_value())
                        {
                            instr.code.JNZ.p_target = labelAddr.value();
                        }
                        else
                        {
                            cerr << "Unknown target [" << label << "] at line_nr " << line_nr << "."
                                 << endl;
                            return 1;
                        }
                    }
                    else
                    {
                        cerr << "Invalid JNZ instruction format at line_nr " << line_nr << "." << endl;
                        return 1;
                    }
                    break;
                }
                case OPCODE_PRINTR:
                {
                    int r_src;
                    if (iss >> r_src)
                    {
                        instr.code.PRINTR.r_src = r_src;
                    }
                    else
                    {
                        cerr << "Invalid PRINTR instruction format at line_nr " << line_nr << "." << endl;
                        return 1;
                    }
                    break;
                }
            }

            cpu->program->push_back(instr);
        }
        else
        {
            cerr << "Unknown mnemonic: " << first_token << endl;
            return 1;
        }
    }

    // todo: file is not closed on error.
    // Close the file
    infile.close();
    return 0;
}
