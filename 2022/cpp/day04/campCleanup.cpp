#include <fstream>
#include <iostream>
#include <sstream>
#include <string>

static void getNumberFromLine(std::string& str, int& startFirst, int& endFirst, int& startSecond, int& endSecond)
{

    std::stringstream ss;
    ss << str;

    std::string tmp;
    ss >> tmp;

    std::stringstream(tmp) >> startFirst;
    ss >> tmp;
    std::stringstream(tmp) >> endFirst;
    ss >> tmp;
    std::stringstream(tmp) >> startSecond;
    ss >> tmp;
    std::stringstream(tmp) >> endSecond;
}

int getStartFirst(const std::string& line)
{
    int number = 0;
    int size = 0;

    for (char c : line) {
        if (c >= '0' and c <= '9') {
            size++;
        }
    }

    return number;
}

void formatLine(std::string& line)
{
    for (char& i : line) {
        if (i == '-' or i == ',') {
            i = ' ';
        }
    }
}

void campCleanup(std::ifstream& fileContent)
{
    int startFirst = 0;
    int endFirst = 0;
    int startSecond = 0;
    int endSecond = 0;
    int score = 0;
    std::string line;

    while (std::getline(fileContent, line)) {
        formatLine(line);
        getNumberFromLine(line, startFirst, endFirst, startSecond, endSecond);

        if ((startFirst <= startSecond and endFirst >= endSecond) or (startSecond <= startFirst and endSecond >= endFirst) or (startFirst >= startSecond and startFirst <= endSecond) or (startSecond >= startFirst and startSecond <= endFirst) or (endFirst >= startSecond and endFirst <= endSecond) or (endSecond >= startFirst and endSecond <= endFirst)) {
            ++score;
        }
    }
    std::cout << "Score : " << score << std::endl;
}

// void campCleanup(std::ifstream& fileContent)
// {
//     int startFirst = 0;
//     int endFirst = 0;
//     int startSecond = 0;
//     int endSecond = 0;
//     int score = 0;
//     std::string line;
//
//     for (line; std::getline(fileContent, line)) {
//         formatLine(line);
//         // std::cout << line << std::endl;
//         getNumberFromLine(line, startFirst, endFirst, startSecond, endSecond);
//
//         // std::cout << startFirst << "-" << endFirst << "," << startSecond << "-" << endSecond << std::endl;
//
//         if ((startFirst <= startSecond and endFirst >= endSecond) or (startSecond <= startFirst and endSecond >= endFirst)) {
//             // std::cout << "=========" << std::endl;
//             // std::cout << line[0] << "-" << line[2] << "," << line[4] << "-" << line[6] << std::endl;
//             ++score;
//         }
//     }
//     std::cout << "Score : " << score << std::endl;
// }
