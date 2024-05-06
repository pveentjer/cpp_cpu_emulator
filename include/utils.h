#ifndef CPU_EMULATOR_UTILS_H
#define CPU_EMULATOR_UTILS_H

template<typename Key, typename Value>
std::optional<Value> mapGet(const std::unordered_map<Key, Value>& map, const Key& key) {
    auto it = map.find(key);
    if (it != map.end()) {
        return it->second;
    } else {
        return std::nullopt;
    }
}

std::string trim(const std::string &str) ;

bool endsWith(const std::string &fullString, const std::string &ending) ;

std::string removeLast(const std::string& s) ;

#endif //CPU_EMULATOR_UTILS_H
