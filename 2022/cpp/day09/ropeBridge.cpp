#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

struct Position {
    int x = 0;
    int y = 0;
};

int getMaxMove(std::vector<std::string>& lines, std::string add, std::string substract)
{
    std::stringstream ss;
    std::string word;
    int move = 0;
    int maxMove = 0;
    char* pEnd{};

    for (std::string& line : lines) {
        ss << line;
        ss >> word;
        if (word == add) {
            ss >> word;
            move += std::strtol(word.c_str(), &pEnd, 10);
            if (maxMove < move) {
                maxMove = move;
            }
        } else if (word == substract) {
            ss >> word;
            move -= std::strtol(word.c_str(), &pEnd, 10);
        }
        ss.str(std::string());
        ss.clear();
        word.clear();
    }

    return std::abs(maxMove);
}

static void initMap(std::vector<std::string>& lines, std::vector<std::string>& map)
{
    std::string mapRow;
    int maxUpMove = getMaxMove(lines, "U", "D");
    int maxRightMove = getMaxMove(lines, "R", "L");
    int maxDownMove = getMaxMove(lines, "D", "U");
    int maxLeftMove = getMaxMove(lines, "L", "R");
    int mapHeight = maxUpMove + maxDownMove + 1;
    int mapWidth = maxRightMove + maxLeftMove + 1;

    // std::cout << maxUpMove << std::endl;
    // std::cout << maxRightMove << std::endl;
    // std::cout << maxDownMove << std::endl;
    // std::cout << maxLeftMove << std::endl;

    for (int i = 0; i < mapWidth; ++i) {
        mapRow.append(".");
    }
    for (int i = 0; i < mapHeight; ++i) {
        map.emplace_back(mapRow);
    }
}

bool isHeadUp(Position& headPosition, Position& tailPosition)
{
    return headPosition.y < tailPosition.y;
}

bool isHeadRight(Position& headPosition, Position& tailPosition)
{
    return headPosition.x > tailPosition.x;
}

bool isHeadDown(Position& headPosition, Position& tailPosition)
{
    return headPosition.y > tailPosition.y;
}

bool isHeadLeft(Position& headPosition, Position& tailPosition)
{
    return headPosition.x < tailPosition.x;
}

bool isOnPosition(Position& firstPosition, Position& secondPosition)
{
    return (firstPosition.x == secondPosition.x and firstPosition.y == secondPosition.y);
}

bool isHeadNear(Position& headPosition, Position& tailPosition)
{
    return ((headPosition.x == tailPosition.x or headPosition.x + 1 == tailPosition.x or headPosition.x - 1 == tailPosition.x) and (headPosition.y == tailPosition.y or headPosition.y + 1 == tailPosition.y or headPosition.y - 1 == tailPosition.y));
}

void displaySpawn(Position& headPosition, Position& tailPosition, Position& spawnPosition, std::vector<std::string>& map)
{
    if (not isOnPosition(headPosition, spawnPosition) and not isOnPosition(tailPosition, spawnPosition)) {
        map[spawnPosition.y][spawnPosition.x] = 's';
    }
}

void addPosition(Position tailPosition, std::vector<Position>& positions)
{
    for (const auto& position : positions) {
        if (tailPosition.x == position.x and tailPosition.y == position.y) {
            return;
        }
    }
    positions.emplace_back(tailPosition);
}

void moveTail(Position& headPosition, Position& tailPosition, std::vector<std::string>& map, char c)
{
    if (isHeadNear(headPosition, tailPosition)) {
        if (not isOnPosition(headPosition, tailPosition)) {
            map[tailPosition.y][tailPosition.x] = c;
        }
        return;
    }

    map[tailPosition.y][tailPosition.x] = '.';

    if (isHeadUp(headPosition, tailPosition)) {
        tailPosition.y -= 1;
    }
    if (isHeadRight(headPosition, tailPosition)) {
        tailPosition.x += 1;
    }
    if (isHeadDown(headPosition, tailPosition)) {
        tailPosition.y += 1;
    }
    if (isHeadLeft(headPosition, tailPosition)) {
        tailPosition.x -= 1;
    }

    map[tailPosition.y][tailPosition.x] = c;
}

void moveHead(Position& headPosition, std::string& line, std::vector<std::string>& map)
{
    map[headPosition.y][headPosition.x] = '.';

    if (line[0] == 'U') {
        headPosition.y -= 1;
    } else if (line[0] == 'R') {
        headPosition.x += 1;
    } else if (line[0] == 'D') {
        headPosition.y += 1;
    } else if (line[0] == 'L') {
        headPosition.x -= 1;
    }

    map[headPosition.y][headPosition.x] = 'H';
}

void displayFinalMap(std::vector<std::string>& lines, std::vector<Position>& positions)
{
    std::vector<std::string> map = {};

    initMap(lines, map);

    for (const auto& position : positions) {
        map[position.y][position.x] = '#';
    }
    map[getMaxMove(lines, "U", "D")][getMaxMove(lines, "L", "R")] = 's';

    for (const auto& line : map) {
        std::cout << line << std::endl;
    }
}

void ropeBridge(std::ifstream& fileContent)
{
    std::vector<std::string> lines = {};
    std::vector<std::string> map = {};
    std::vector<Position> positions = {};
    std::stringstream ss;
    std::string word;
    int repeat = 0;

    for (std::string line; std::getline(fileContent, line);) {
        lines.emplace_back(line);
    }

    Position headPosition = {.x = getMaxMove(lines, "L", "R"), .y = getMaxMove(lines, "U", "D")};
    Position firstBodyPosition = {.x = headPosition.x, .y = headPosition.y};
    Position secondBodyPosition = {.x = headPosition.x, .y = headPosition.y};
    Position thirdBodyPosition = {.x = headPosition.x, .y = headPosition.y};
    Position fourthBodyPosition = {.x = headPosition.x, .y = headPosition.y};
    Position fifthBodyPosition = {.x = headPosition.x, .y = headPosition.y};
    Position sixthBodyPosition = {.x = headPosition.x, .y = headPosition.y};
    Position seventhBodyPosition = {.x = headPosition.x, .y = headPosition.y};
    Position eighthBodyPosition = {.x = headPosition.x, .y = headPosition.y};
    Position ninthBodyPosition = {.x = headPosition.x, .y = headPosition.y};
    Position spawnPosition = {.x = headPosition.x, .y = headPosition.y};

    initMap(lines, map);

    // std::cout << headPosition.y << std::endl;
    // std::cout << headPosition.x << std::endl;
    map[headPosition.y][headPosition.x] = 'H';

    for (auto& line : lines) {
        ss << line;
        ss >> word;
        ss >> word;
        repeat = std::stoi(word.c_str());
        for (int i = 0; i < repeat; ++i) {
            // std::cout << "===========" << std::endl;
            // for (std::string& ma : map) {
            //     std::cout << ma << std::endl;
            // }
            moveHead(headPosition, line, map);
            moveTail(headPosition, firstBodyPosition, map, '1');
            moveTail(firstBodyPosition, secondBodyPosition, map, '2');
            moveTail(secondBodyPosition, thirdBodyPosition, map, '3');
            moveTail(thirdBodyPosition, fourthBodyPosition, map, '4');
            moveTail(fourthBodyPosition, fifthBodyPosition, map, '5');
            moveTail(fifthBodyPosition, sixthBodyPosition, map, '6');
            moveTail(sixthBodyPosition, seventhBodyPosition, map, '7');
            moveTail(seventhBodyPosition, eighthBodyPosition, map, '8');
            moveTail(eighthBodyPosition, ninthBodyPosition, map, '9');
            addPosition(ninthBodyPosition, positions);
            displaySpawn(headPosition, thirdBodyPosition, spawnPosition, map);
        }
        ss.str(std::string());
        ss.clear();
        word.clear();
    }
    // std::cout << "===========" << std::endl;
    // for (std::string& ma : map) {
    //     std::cout << ma << std::endl;
    // }
    // std::cout << "===========" << std::endl;
    displayFinalMap(lines, positions);
    std::cout << positions.size() << std::endl;
}

// void ropeBridge(std::ifstream& fileContent)
// {
//     std::vector<std::string> lines = {};
//     std::vector<std::string> map = {};
//     std::vector<Position> positions = {};
//     std::stringstream ss;
//     std::string word;
//     int repeat = 0;
//
//     for (std::string line; std::getline(fileContent, line);) {
//         lines.emplace_back(line);
//     }
//
//     Position headPosition = {.x = getMaxMove(lines, "L", "R"), .y = getMaxMove(lines, "U", "D")};
//     Position tailPosition = {.x = getMaxMove(lines, "L", "R"), .y = getMaxMove(lines, "U", "D")};
//     Position spawnPosition = {.x = getMaxMove(lines, "L", "R"), .y = getMaxMove(lines, "U", "D")};
//
//     initMap(lines, map);
//
//     // std::cout << headPosition.y << std::endl;
//     // std::cout << headPosition.x << std::endl;
//     map[headPosition.y][headPosition.x] = 'H';
//
//     for (auto& line : lines) {
//         ss << line;
//         ss >> word;
//         ss >> word;
//         repeat = std::atoi(word.c_str());
//         for (int i = 0; i < repeat; ++i) {
//             // std::cout << "===========" << std::endl;
//             // for (std::string& ma : map) {
//             //     std::cout << ma << std::endl;
//             // }
//             moveHead(headPosition, line, map);
//             moveTail(headPosition, tailPosition, map);
//             addPosition(tailPosition, positions);
//             displaySpawn(headPosition, tailPosition, spawnPosition, map);
//         }
//         ss.str(std::string());
//         ss.clear();
//         word.clear();
//     }
//     // std::cout << "===========" << std::endl;
//     // for (std::string& ma : map) {
//     //     std::cout << ma << std::endl;
//     // }
//     // std::cout << "===========" << std::endl;
//     displayFinalMap(lines, positions);
//     std::cout << positions.size() << std::endl;
// }
