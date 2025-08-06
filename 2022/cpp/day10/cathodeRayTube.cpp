#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

int getMaxCycles(std::vector<std::string>& lines)
{
    std::stringstream ss;
    std::string word;
    int nbCycles = 0;

    for (auto& line : lines) {
        ss << line;
        ss >> word;
        if (word == "addx") {
            nbCycles += 2;
        } else if (word == "noop") {
            ++nbCycles;
        }
        ss.str(std::string());
        ss.clear();
        word.clear();
    }
    return nbCycles;
}

void cathodeRayTube(std::ifstream& fileContent)
{
    std::vector<std::string> lines = {};
    std::vector<std::string> drawing = {
        "########################################",
        "########################################",
        "########################################",
        "########################################",
        "########################################",
        "########################################"};
    std::stringstream ss;
    std::string word;
    int registerX = 1;
    int signal = 0;
    int y = 0;

    for (std::string line; std::getline(fileContent, line);) {
        lines.emplace_back(line);
    }

    int maxCycles = getMaxCycles(lines);
    auto it = lines.begin();

    for (int cycle = 1; cycle < maxCycles; ++cycle) {
        ss << *it;
        ss >> word;
        if (cycle % 40 == 0) {
            ++y;
        }

        if (word == "addx") {
            for (int i = 0; i < 1; ++i) {
                if (registerX not_eq cycle % 40 && registerX + 1 not_eq cycle % 40 && registerX - 1 not_eq cycle % 40) {
                    drawing[y][cycle % 40] = '.';
                }
                ++cycle;
                if (cycle % 40 == 0) {
                    ++y;
                }
            }
            ss >> word;
            registerX += std::stoi(word.c_str());
        }
        if (registerX not_eq cycle % 40 && registerX + 1 not_eq cycle % 40 && registerX - 1 not_eq cycle % 40) {
            drawing[y][cycle % 40] = '.';
        }

        ++it;
        ss.str(std::string());
        ss.clear();
        word.clear();
    }
    for (auto& line : drawing) {
        std::cout << line << std::endl;
    }
}

// void cathodeRayTube(std::ifstream& fileContent)
// {
//     std::vector<std::string> lines = {};
//     std::stringstream ss;
//     std::string word;
//     int x = 1;
//     int signal = 0;
//
//     for (std::string line; std::getline(fileContent, line);) {
//         lines.emplace_back(line);
//     }
//
//     int maxCycles = getMaxCycles(lines);
//
//     // std::cout << maxCycles << std::endl;
//
//     auto it = lines.begin();
//
//     for (int cycle = 1; cycle < maxCycles; ++cycle) {
//         ss << *it;
//         ss >> word;
//         if (cycle == 20 or cycle == 60 or cycle == 100 or cycle == 140 or cycle == 180 or cycle == 220) {
//             std::cout << "Cycle : " << cycle << " : " << x << std::endl;
//             signal += cycle * x;
//         }
//         // std::cout << "Start Cycle : " << cycle << ", X = " << x << std::endl;
//
//         if (word == "addx") {
//             for (int i = 0; i < 1; ++i) {
//                 // std::cout << "End Cycle, X = " << x << std::endl;
//                 ++cycle;
//                 if (cycle == 20 or cycle == 60 or cycle == 100 or cycle == 140 or cycle == 180 or cycle == 220) {
//                     std::cout << "Cycle " << cycle << " : " << x << std::endl;
//                     signal += cycle * x;
//                 }
//                 // std::cout << "Start Cycle, X = " << x << std::endl;
//             }
//             ss >> word;
//             // std::cout << "=========" << std::endl;
//             // std::cout << x << " + " << std::atoi(word.c_str()) << std::endl;
//             x += std::atoi(word.c_str());
//         }
//
//         ++it;
//         ss.str(std::string());
//         ss.clear();
//         word.clear();
//         // std::cout << "End Cycle, X = " << x << std::endl;
//     }
//     std::cout << "Signal : " << signal << std::endl;
// }
