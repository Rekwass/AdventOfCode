#include <cstddef>
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

struct Position {
    int x;
    int y;
};

void drawWallRight(std::vector<std::string>& map, Position& position, Position& nextPosition)
{
    for (int i = position.x; i <= nextPosition.x; ++i) {
        map.at(position.y).at(i) = '#';
    }
}

void drawWallLeft(std::vector<std::string>& map, Position& position, Position& nextPosition)
{
    for (int i = position.x; i >= nextPosition.x; --i) {
        map.at(position.y).at(i) = '#';
    }
}

void drawWallDown(std::vector<std::string>& map, Position& position, Position& nextPosition)
{
    for (int i = position.y; i <= nextPosition.y; ++i) {
        map.at(i).at(position.x) = '#';
    }
}

void drawWallUp(std::vector<std::string>& map, Position& position, Position& nextPosition)
{
    for (int i = position.y; i >= nextPosition.y; --i) {
        map.at(i).at(position.x) = '#';
    }
}

void drawWall(std::vector<std::string>& map, Position& position, Position& nextPosition)
{
    if (position.x < nextPosition.x) {
        drawWallRight(map, position, nextPosition);
    } else if (position.x > nextPosition.x) {
        drawWallLeft(map, position, nextPosition);
    } else if (position.y < nextPosition.y) {
        drawWallDown(map, position, nextPosition);
    } else if (position.y > nextPosition.y) {
        drawWallUp(map, position, nextPosition);
    }
}

static void addWalls(std::vector<std::string>& map, std::vector<std::vector<Position>>& positionLists)
{
    Position position{.x = 500, .y = 0};
    Position nextPosition{.x = 500, .y = 0};

    for (auto& positionList : positionLists) {
        if (positionList.size() < 2) {
            continue;
        }
        for (int i = 0; i < positionList.size() - 1; ++i) {
            position = positionList.at(i);
            nextPosition = positionList.at(i + 1);
            drawWall(map, position, nextPosition);
        }
    }
}

static std::vector<std::string> createMap(std::vector<std::vector<Position>>& positionsLists, const int smallestX, const Position& biggestPosition)
{
    int mapWidth = (biggestPosition.y + 20) * 2;

    std::cout << "mapWidth = " << mapWidth << std::endl;

    std::string line(mapWidth, '.');
    std::vector<std::string> map(biggestPosition.y + 1, line);

    addWalls(map, positionsLists);
    map.at(0).at(500 - smallestX + biggestPosition.y) = '+';

    return map;
}

bool isUnderFree(std::vector<std::string>& map, Position& sandPosition)
{
    return (sandPosition.y < map.size() and map.at(sandPosition.y + 1).at(sandPosition.x) == '.');
}

bool isBottomLeftFree(std::vector<std::string>& map, Position& sandPosition)
{
    return (sandPosition.x < map.at(0).length() and map.at(sandPosition.y + 1).at(sandPosition.x + 1) == '.');
}

bool isBottomRightFree(std::vector<std::string>& map, Position& sandPosition)
{
    return (sandPosition.x > 0 and map.at(sandPosition.y + 1).at(sandPosition.x - 1) == '.');
}

bool isSandStuck(std::vector<std::string>& map, Position& sandPosition)
{
    return not(isUnderFree(map, sandPosition) or isBottomLeftFree(map, sandPosition) or isBottomRightFree(map, sandPosition));
}

void moveSand(std::vector<std::string>& map, Position& sandPosition)
{
    while (not isSandStuck(map, sandPosition)) {
        // std::cout << "SandPosition : [" << sandPosition.x << ", " << sandPosition.y << "]" << std::endl;
        // std::cout << "map.size() = " << map.size() << std::endl;
        // std::cout << "map.at(0).length() = " << map.at(0).length() << std::endl;
        map.at(sandPosition.y).at(sandPosition.x) = '.';
        if (map.at(sandPosition.y + 1).at(sandPosition.x) == '.') {
        } else if (map.at(sandPosition.y + 1).at(sandPosition.x - 1) == '.') {
            --sandPosition.x;
        } else if (map.at(sandPosition.y + 1).at(sandPosition.x + 1) == '.') {
            ++sandPosition.x;
        }
        ++sandPosition.y;
        map.at(sandPosition.y).at(sandPosition.x) = 'o';
    }
}

bool compareMaps(std::vector<std::string>& map, std::vector<std::string>& mapAfter)
{
    for (int i = 0; i < map.size() - 1; ++i) {
        for (int j = 0; j < map.at(0).length() - 1; ++j) {
            if (map.at(i)[j] not_eq mapAfter.at(i)[j]) {
                return false;
            }
        }
    }
    return true;
}

bool isMapFilled(std::vector<std::string>& map, std::vector<std::string> mapAfter, Position sandPosition)
{
    moveSand(mapAfter, sandPosition);

    return compareMaps(map, mapAfter);
}

static void displayMap(std::vector<std::string>& map)
{
    for (auto& line : map) {
        std::cout << line << std::endl;
    }
}

void fillMap(std::vector<std::string>& map, const int smallestX, Position& biggestPosition)
{
    Position sandPosition{.x = 500 - smallestX + biggestPosition.y, .y = 0};

    while (not isMapFilled(map, map, sandPosition)) {
        moveSand(map, sandPosition);
        sandPosition = {500 - smallestX + biggestPosition.y, 0};
        // displayMap(map);
        // std::cout << std::endl;
    }
}

int countSand(std::vector<std::string>& map)
{
    int nbSandUnit = 0;

    for (auto& line : map) {
        for (auto& character : line) {
            if (character == 'o') {
                ++nbSandUnit;
            }
        }
    }

    return nbSandUnit;
}

static void getInputFromStdin(std::ifstream& fileContent, std::vector<std::string>& lines)
{
    for (std::string line; std::getline(fileContent, line);) {
        lines.emplace_back(line);
    }
}

static void removeSubStringFromString(std::string& string, const std::string_view& subString)
{
    size_t pos = 0;

    pos = string.find(subString);
    while (pos not_eq std::string::npos) {
        string.erase(pos, subString.length());
        pos = string.find(subString);
    }
}

static void replaceCharFromString(std::string& string, const char charToRemove, const char charToAdd)
{
    size_t pos = 0;

    pos = string.find(charToRemove);
    while (pos not_eq std::string::npos) {
        string.at(pos) = charToAdd;
        pos = string.find(charToRemove);
    }
}

static std::vector<Position> addPositions(std::string& line)
{
    std::stringstream ss;
    std::string word;
    std::vector<Position> positions;
    Position position = {.x = 500, .y = 0};

    ss << line;

    while (not ss.eof()) {
        ss >> word;
        position.x = std::stoi(word);
        ss >> word;
        position.y = std::stoi(word);
        positions.emplace_back(position);
        word.clear();
    }
    return positions;
}

static void parseInput(std::vector<std::string>& lines, std::vector<std::vector<Position>>& positionsLists)
{

    for (auto& line : lines) {
        removeSubStringFromString(line, " ->");
        replaceCharFromString(line, ',', ' ');
        positionsLists.emplace_back(addPositions(line));
    }
}

static int getSmallestX(std::vector<std::vector<Position>>& positionLists)
{
    int x = 500;

    for (auto& positionList : positionLists) {
        for (auto& position : positionList) {
            if (position.x < x) {
                x = position.x;
            }
        }
    }

    return x;
}

static Position getBiggestPosition(std::vector<std::vector<Position>>& positionLists)
{
    Position biggestPosition = {.x = 0, .y = 0};

    for (auto& positionList : positionLists) {
        for (auto& position : positionList) {
            if (biggestPosition.x < position.x) {
                biggestPosition.x = position.x;
            }
            if (biggestPosition.y < position.y) {
                biggestPosition.y = position.y;
            }
        }
    }

    return biggestPosition;
}

void fixPositionLists(std::vector<std::vector<Position>>& positionLists, const int smallestX)
{
    for (auto& positionList : positionLists) {
        for (auto& position : positionList) {
            position.x -= smallestX;
        }
    }
}

void fixPositionLists2(std::vector<std::vector<Position>>& positionLists, Position& biggestPosition)
{
    for (auto& positionList : positionLists) {
        for (auto& position : positionList) {
            position.x += biggestPosition.y;
        }
    }
}

void regolithReservoir(std::ifstream& fileContent)
{
    std::vector<std::string> map;
    std::vector<std::string> lines;
    std::vector<std::vector<Position>> positionLists;
    int smallestX = 500;
    Position biggestPosition = {.x = 500, .y = 0};

    getInputFromStdin(fileContent, lines);
    parseInput(lines, positionLists);

    smallestX = getSmallestX(positionLists);
    fixPositionLists(positionLists, smallestX);
    biggestPosition = getBiggestPosition(positionLists);
    biggestPosition.y += 2;
    fixPositionLists2(positionLists, biggestPosition);

    map = createMap(positionLists, smallestX, biggestPosition);
    for (int i = 0; i < map.at(0).length(); ++i) {
        map.at(biggestPosition.y)[i] = '#';
    }

    try {
        fillMap(map, smallestX, biggestPosition);
    } catch (const std::out_of_range& e) {
    }

    map.at(0).at(500 - smallestX + biggestPosition.y) = 'o';

    std::cout << "CountSand : " << countSand(map) << std::endl;

    for (auto& line : map) {
        std::cout << line << std::endl;
    }
}

// void regolithReservoir(std::ifstream& fileContent)
// {
//     std::vector<std::string> map;
//     std::vector<std::string> lines;
//     std::vector<std::vector<Position>> positionLists;
//     int smallestX = 500;
//     Position biggestPosition = {.x = 500, .y = 0};
//
//     getInputFromStdin(fileContent, lines);
//     parseInput(lines, positionLists);
//
//     smallestX = getSmallestX(positionLists);
//     fixPositionLists(positionLists, smallestX);
//     biggestPosition = getBiggestPosition(positionLists);
//
//     map = createMap(positionLists, smallestX, biggestPosition);
//     try {
//         fillMap(map, smallestX);
//     } catch (const std::out_of_range& e) {
//         std::cout << "CountSand : " << countSand(map) << std::endl;
//     }
//
//     for (auto& line : map) {
//         std::cout << line << std::endl;
//     }
// }
