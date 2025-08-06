#include <fstream>
#include <iostream>
#include <string>
#include <vector>

struct Position {
    int x = 0;
    int y = 0;
};

void initPathMap(std::vector<std::vector<int>>& pathMap, std::vector<std::string>& heightMap)
{
    int mapWidth = heightMap.at(0).length();
    int mapHeight = heightMap.size();
    std::vector<int> line = {};

    line.reserve(mapWidth);
    for (int i = 0; i < mapWidth; ++i) {
        line.emplace_back(-1);
    }
    pathMap.reserve(mapHeight);
    for (int i = 0; i < mapHeight; ++i) {
        pathMap.emplace_back(line);
    }
}

void findPosition(std::vector<std::string>& heightMap, Position& position, const char c)
{
    for (int y = 0; y < heightMap.size(); ++y) {
        for (int x = 0; x < heightMap.at(0).length(); ++x) {
            if (heightMap.at(y)[x] == c) {
                position.x = x;
                position.y = y;
                return;
            }
        }
    }
}

void expandPathDirection(std::vector<std::string>& heightMap, std::vector<std::vector<int>>& pathMap, int x, int y, int mapHeight, int mapWidth, int shortestPath)
{
    // std::cout << "x = " << x << ", y = " << y << std::endl;
    // std::cout << heightMap.at(y)[x] << std::endl;
    if (x + 1 < mapWidth and heightMap.at(y)[x] >= heightMap.at(y)[x + 1] - 1 and pathMap.at(y).at(x + 1) == -1) {
        // std::cout << "RIGHT" << std::endl;
        pathMap.at(y).at(x + 1) = shortestPath + 1;
    }
    if (y + 1 < mapHeight and heightMap.at(y)[x] >= heightMap.at(y + 1)[x] - 1 and pathMap.at(y + 1).at(x) == -1) {
        // std::cout << "DOWN" << std::endl;
        pathMap.at(y + 1).at(x) = shortestPath + 1;
    }
    if (x - 1 >= 0 and heightMap.at(y)[x] >= heightMap.at(y)[x - 1] - 1 and pathMap.at(y).at(x - 1) == -1) {
        // std::cout << "LEFT" << std::endl;
        pathMap.at(y).at(x - 1) = shortestPath + 1;
    }
    if (y - 1 >= 0 and heightMap.at(y)[x] >= heightMap.at(y - 1)[x] - 1 and pathMap.at(y - 1).at(x) == -1) {
        // std::cout << "UP" << std::endl;
        pathMap.at(y - 1).at(x) = shortestPath + 1;
    }
}

void spreadPath(std::vector<std::string>& heightMap, std::vector<std::vector<int>>& pathMap, Position& endPosition, int shortestPath)
{
    int mapWidth = heightMap.at(0).length();
    int mapHeight = heightMap.size();

    for (int y = 0; y < mapHeight; ++y) {
        for (int x = 0; x < mapWidth; ++x) {
            // std::cout << "y = " << y << ", x = " << x << std::endl;
            // std::cout << "pathmap[y][x]" << pathMap.at(y).at(x) << std::endl;
            if (pathMap.at(y).at(x) == shortestPath) {
                // std::cout << "IN !" << std::endl;
                expandPathDirection(heightMap, pathMap, x, y, mapHeight, mapWidth, shortestPath);
            }
        }
    }
}

bool isFinished(std::vector<std::vector<int>>& pathMap, Position& endPosition)
{
    return pathMap.at(endPosition.y).at(endPosition.x) not_eq -1;
}

int findShortestPath(std::vector<std::string>& heightMap, std::vector<std::vector<int>>& pathMap, Position& endPosition)
{
    int shortestPath = 0;
    int i = 0;

    while (not isFinished(pathMap, endPosition)) {
        spreadPath(heightMap, pathMap, endPosition, shortestPath);
        ++shortestPath;
    }

    return shortestPath;
}

// void hillClimbingAlgorithm(std::ifstream fileContent)
// {
//     std::vector<std::string> heightMap = {};
//     std::vector<std::vector<int>> pathMap = {};
//     Position position = {};
//     Position endPosition = {};
//     int shortestPath = 0;
//
//     for (std::string line; std::getline(fileContent, line);) {
//         heightMap.emplace_back(line);
//     }
//
//     initPathMap(pathMap, heightMap);
//     findPosition(heightMap, endPosition, 'E');
//     heightMap.at(endPosition.y).at(endPosition.x) = '{';
//
//     for (int y = 0; y < heightMap.size(); ++y) {
//         for (int x = 0; x < heightMap.at(0).length(); ++x) {
//             if (heightMap.at(y)[x] == 'a') {
//                 pathMap.at(y).at(x) = 0;
//             }
//         }
//     }
//
//     // std::cout << "Spawn : y = " << position.y << ", x = " << position.x << std::endl;
//     // std::cout << "END : y = " << endPosition.y << ", x = " << endPosition.x << std::endl;
//
//     // for (const auto& line : heightMap) {
//     //     std::cout << line << std::endl;
//     // }
//
//     // std::cout << " =============== " << std::endl;
//     // std::string prefix;
//     // for (const auto& line : pathMap) {
//     //     std::cout << "[";
//     //     for (const auto& charater : line) {
//     //         std::cout << prefix << charater;
//     //         prefix = ",";
//     //     }
//     //     prefix = "";
//     //     std::cout << "]" << std::endl;
//     // }
//
//     shortestPath = findShortestPath(heightMap, pathMap, endPosition);
//
//     std::cout << "ShortestPath = " << shortestPath << std::endl;
// }

void applySolution(std::vector<std::string>& heightMap, std::vector<std::vector<int>>& pathMap, Position& position)
{
    int shortestPath = pathMap.at(position.y).at(position.x);
    int mapWidth = heightMap.at(0).length();
    int mapHeight = heightMap.size();

    for (; shortestPath > 0; --shortestPath) {
        if (position.x + 1 < mapWidth and pathMap.at(position.y)[position.x + 1] == shortestPath) {
            // std::cout << "RIGHT" << std::endl;
            heightMap.at(position.y).at(position.x) = '<';
            ++position.x;
        } else if (position.y + 1 < mapHeight and pathMap.at(position.y + 1)[position.x] == shortestPath) {
            // std::cout << "DOWN" << std::endl;
            heightMap.at(position.y).at(position.x) = '^';
            ++position.y;
        } else if (position.x - 1 >= 0 and pathMap.at(position.y)[position.x - 1] == shortestPath) {
            // std::cout << "LEFT" << std::endl;
            heightMap.at(position.y).at(position.x) = '>';
            --position.x;
        } else if (position.y - 1 >= 0 and pathMap.at(position.y - 1)[position.x] == shortestPath) {
            // std::cout << "UP" << std::endl;
            heightMap.at(position.y).at(position.x) = 'v';
            --position.y;
        }
    }
}

void hillClimbingAlgorithm(std::ifstream& fileContent)
{
    std::vector<std::string> heightMap = {};
    std::vector<std::vector<int>> pathMap = {};
    Position position = {};
    Position endPosition = {};
    int shortestPath = 0;

    for (std::string line; std::getline(fileContent, line);) {
        heightMap.emplace_back(line);
    }

    initPathMap(pathMap, heightMap);
    findPosition(heightMap, position, 'S');
    findPosition(heightMap, endPosition, 'E');
    heightMap.at(position.y)[position.x] = '`';
    heightMap.at(endPosition.y).at(endPosition.x) = '{';
    pathMap.at(position.y).at(position.x) = 0;

    // std::cout << "Spawn : y = " << position.y << ", x = " << position.x << std::endl;
    // std::cout << "END : y = " << endPosition.y << ", x = " << endPosition.x << std::endl;

    // for (const auto& line : heightMap) {
    //     std::cout << line << std::endl;
    // }

    // std::cout << " =============== " << std::endl;
    // std::string prefix;
    // for (const auto& line : pathMap) {
    //     std::cout << "[";
    //     for (const auto& charater : line) {
    //         std::cout << prefix << charater;
    //         prefix = ",";
    //     }
    //     prefix = "";
    //     std::cout << "]" << std::endl;
    // }

    shortestPath = findShortestPath(heightMap, pathMap, endPosition);

    for (auto& line : heightMap) {
        for (auto& charater : line) {
            charater = '.';
        }
    }

    heightMap.at(endPosition.y)[endPosition.x] = 'E';
    applySolution(heightMap, pathMap, endPosition);

    for (auto& line : heightMap) {
        std::cout << line << std::endl;
    }

    std::cout << "ShortestPath = " << shortestPath << std::endl;
}
