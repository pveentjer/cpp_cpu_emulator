//
// Created by pveentjer on 4/29/24.
//

#include <string>
#include <algorithm>
#include <unordered_map>
#include <optional>
#include "include/utils.h"


std::string trim(const std::string &str)
{
    auto start = std::find_if_not(str.begin(), str.end(), [](unsigned char c) { return std::isspace(c); });
    auto end = std::find_if_not(str.rbegin(), str.rend(), [](unsigned char c) { return std::isspace(c); }).base();
    return (start < end ? std::string(start, end) : "");
}

bool endsWith(const std::string &fullString, const std::string &ending)
{
    if (fullString.length() >= ending.length())
    {
        return (fullString.compare(fullString.length() - ending.length(), ending.length(), ending) == 0);
    }
    else
    {
        return false;
    }
}

std::string removeLast(const std::string &s)
{
    if (s.length() > 0)
    {
        return s.substr(0, s.length() - 1);
    }
    else
    {
        return "";
    }
}