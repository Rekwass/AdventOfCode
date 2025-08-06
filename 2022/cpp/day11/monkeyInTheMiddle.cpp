#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

struct Monkey {
    std::vector<unsigned long long> items;
    std::string firstNumber;
    std::string secondNumber;
    char operation;
    int divideBy;
    int condTrue;
    int condFalse;
    int nbInspection;
};

void addItems(std::vector<Monkey>& monkeys, int i, std::stringstream& ss, std::string& word)
{
    int number = 0;

    while (!ss.eof()) {
        ss >> word;
        if (std::stringstream(word) >> number) {
            monkeys.at(i).items.emplace_back(number);
        }
        word.clear();
    }
}

void addOperation(std::vector<Monkey>& monkeys, int i, std::stringstream& ss, std::string& word)
{
    ss >> word;
    ss >> word;
    ss >> word;
    monkeys.at(i).firstNumber = word;
    ss >> word;
    monkeys.at(i).operation = word[0];
    ss >> word;
    monkeys.at(i).secondNumber = word;
}

void addDivisable(std::vector<Monkey>& monkeys, int i, std::stringstream& ss, std::string& word)
{
    char* pEnd{};
    ss >> word;
    ss >> word;
    ss >> word;
    monkeys.at(i).divideBy = std::strtol(word.c_str(), &pEnd, 10);
}

void addTrue(std::vector<Monkey>& monkeys, int i, std::stringstream& ss, std::string& word)
{
    char* pEnd{};
    ss >> word;
    ss >> word;
    ss >> word;
    ss >> word;
    monkeys.at(i).condTrue = std::strtol(word.c_str(), &pEnd, 10);
}

void addFalse(std::vector<Monkey>& monkeys, int i, std::stringstream& ss, std::string& word)
{
    char* pEnd{};
    ss >> word;
    ss >> word;
    ss >> word;
    ss >> word;
    monkeys.at(i).condFalse = std::strtol(word.c_str(), &pEnd, 10);
}

void initMonkeys(std::vector<std::string>& lines, std::vector<Monkey>& monkeys)
{
    std::stringstream ss;
    std::string word;
    int i = -1;

    for (auto& line : lines) {
        ss << line;
        ss >> word;

        if (word == "Monkey") {
            monkeys.emplace_back(Monkey{.nbInspection = 0});
            ++i;
        } else if (word == "Starting") {
            addItems(monkeys, i, ss, word);
        } else if (word == "Operation:") {
            addOperation(monkeys, i, ss, word);
        } else if (word == "Test:") {
            addDivisable(monkeys, i, ss, word);
        } else if (word == "If") {
            ss >> word;
            if (word == "true:") {
                addTrue(monkeys, i, ss, word);
            } else if (word == "false:") {
                addFalse(monkeys, i, ss, word);
            }
        }

        ss.str(std::string());
        ss.clear();
        word.clear();
    }

    // std::string prefix;
    // int j = 0;
    // for (auto& monkey : monkeys) {
    //     std::cout << "========= Monkey " << j << " =========" << std::endl;
    //     std::cout << "Items : [";
    //     for (auto& item : monkey.items) {
    //         std::cout << prefix << item;
    //         prefix = ", ";
    //     }
    //     prefix.clear();
    //     std::cout << "]" << std::endl;
    //     std::cout << "Operation : " << monkey.firstNumber << " " << monkey.operation << " " << monkey.secondNumber << std::endl;
    //     std::cout << "Test : Divisable by " << monkey.divideBy << std::endl;
    //     std::cout << "If true : throw to monkey " << monkey.condTrue << std::endl;
    //     std::cout << "If false : throw to monkey " << monkey.condFalse << std::endl;
    //     ++j;
    // }
}

unsigned long long calculateNumbers(unsigned long long a, unsigned long long b, char op)
{
    unsigned long long result = 0;

    if (op == '+') {
        // std::cout << "    Worry level increases by " << b << " to " << a + b << "." << std::endl;
        result = a + b;
    } else if (op == '-') {
        result = a - b;
    } else if (op == '*') {
        // std::cout << "    Worry level is multiplied by " << b << " to " << a * b << "." << std::endl;
        result = a * b;
    } else if (op == '/' and b not_eq 0) {
        result = a / b;
    }
    return result;
}

unsigned long long calculateWorryLevel(int item, std::string& firstNumber, char operation, std::string& secondNumber)
{
    unsigned long long worryLevel = 0;
    char* pEnd{};

    if (firstNumber == "old" and secondNumber not_eq "old") {
        worryLevel = calculateNumbers(item, std::stoi(secondNumber), operation);
    } else if (firstNumber not_eq "old" and secondNumber == "old") {
        worryLevel = calculateNumbers(std::stoi(firstNumber), item, operation);
    } else if (firstNumber == "old" and secondNumber == "old") {
        worryLevel = calculateNumbers(item, item, operation);
    } else if (firstNumber not_eq "old" and secondNumber not_eq "old") {
        worryLevel = calculateNumbers(std::stoi(firstNumber), std::stoi(secondNumber), operation);
    }

    return worryLevel;
}

// bool isDivisible(int item, int divider)
// {
//     int tmp = divider;
//
//     if (tmp == item) {
//         return true;
//     }
//     while (tmp < item) {
//         tmp += divider;
//         if (tmp == item) {
//             return true;
//         }
//     }
//     return false;
// }

void monkeyInTheMiddle(std::ifstream& fileContent)
{
    std::vector<std::string> lines = {};
    std::vector<Monkey> monkeys = {};
    std::stringstream ss;
    std::string word;
    int nbRound = 10000;

    for (std::string line; std::getline(fileContent, line);) {
        lines.emplace_back(line);
    }

    initMonkeys(lines, monkeys);

    int modulo = 1;
    for (auto& monkey : monkeys) {
        modulo *= monkey.divideBy;
    }
    int d = 0;
    for (int round = 0; round < nbRound; ++round) {
        for (auto& monkey : monkeys) {
            // std::cout << "Monkey " << d << ":" << std::endl;
            for (auto it = monkey.items.begin(); it not_eq monkey.items.end();) {
                // std::cout << "  Monkey inspects an item with a worry level of " << *it << "." << std::endl;
                ++monkey.nbInspection;
                *it = calculateWorryLevel(*it, monkey.firstNumber, monkey.operation, monkey.secondNumber);
                // *it /= 3;
                *it %= modulo;
                if (*it % monkey.divideBy == 0) {
                    // std::cout << "    Current worry level is not divisible by " << monkey.divideBy << "." << std::endl;
                    // std::cout << "    Item with worry level " << *it << " is thrown to monkey " << monkey.condTrue << "." << std::endl;
                    monkeys.at(monkey.condTrue).items.emplace_back(*it);
                } else {
                    // std::cout << "    Current worry level is not divisible by " << monkey.divideBy << "." << std::endl;
                    // std::cout << "    Item with worry level " << *it << " is thrown to monkey " << monkey.condFalse << "." << std::endl;
                    monkeys.at(monkey.condFalse).items.emplace_back(*it);
                }
                it = monkey.items.erase(it);
            }
            ++d;
        }
        d = 0;
    }

    d = 0;

    unsigned long long first = 0;
    unsigned long long second = 0;
    unsigned long long maxInspection = 0;

    for (auto& monkey : monkeys) {
        std::cout << "Nb inspection Monkey : " << monkey.nbInspection << std::endl;
    }
    for (auto& monkey : monkeys) {
        if (monkey.nbInspection > maxInspection) {
            maxInspection = monkey.nbInspection;
        }
    }
    for (auto it = monkeys.begin(); it != monkeys.end(); ++it) {
        if (it->nbInspection == maxInspection) {
            first = maxInspection;
            monkeys.erase(it);
            break;
        }
        std::cout << "====== Monkey " << d << " ======" << std::endl;
        std::cout << "[" << it->nbInspection << "]" << std::endl;
    }
    maxInspection = 0;
    for (auto& monkey : monkeys) {
        if (monkey.nbInspection > maxInspection) {
            maxInspection = monkey.nbInspection;
        }
    }
    for (auto it = monkeys.begin(); it != monkeys.end(); ++it) {
        if (it->nbInspection == maxInspection) {
            second = maxInspection;
            monkeys.erase(it);
            break;
        }
        std::cout << "====== Monkey " << d << " ======" << std::endl;
        std::cout << "[" << it->nbInspection << "]" << std::endl;
    }

    unsigned long long result = first * second;
    std::cout << "first = " << first << std::endl;
    std::cout << "second = " << second << std::endl;
    std::cout << "result = " << first * second << std::endl;

    // std::string prefix;
    // int j = 0;
    // for (auto& monkey : monkeys) {
    //     std::cout << "========= Monkey " << j << " =========" << std::endl;
    //     std::cout << "Items : [";
    //     for (auto& item : monkey.items) {
    //         std::cout << prefix << item;
    //         prefix = ", ";
    //     }
    //     prefix.clear();
    //     std::cout << "]" << std::endl;
    //     std::cout << "Operation : " << monkey.firstNumber << " " << monkey.operation << " " << monkey.secondNumber << std::endl;
    //     std::cout << "Test : Divisable by " << monkey.divideBy << std::endl;
    //     std::cout << "If true : throw to monkey " << monkey.condTrue << std::endl;
    //     std::cout << "If false : throw to monkey " << monkey.condFalse << std::endl;
    //     ++j;
    // }
}
