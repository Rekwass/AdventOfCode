#include <deque>
#include <fstream>
#include <iostream>
#include <sstream>

struct Position {
    int x;
    int y;
};

static void getInputFromStdin(std::ifstream& fileContent, std::deque<std::string>& lines)
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

static std::deque<Position> addPositions(std::string& line)
{
    std::stringstream ss;
    std::string word;
    std::deque<Position> positions;
    Position position = {.x = 0, .y = 0};

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

static void parseInput(std::deque<std::string>& lines, std::deque<std::deque<Position>>& positionsLists)
{

    for (auto& line : lines) {
        removeSubStringFromString(line, "Sensor at x=");
        removeSubStringFromString(line, ",");
        removeSubStringFromString(line, "y=");
        removeSubStringFromString(line, ":");
        removeSubStringFromString(line, "closest beacon is at x=");
        positionsLists.emplace_back(addPositions(line));
    }
}

static void getSmallestPosition(std::deque<std::deque<Position>>& positionLists, Position& smallestPosition)
{
    for (auto& positionList : positionLists) {
        for (auto& position : positionList) {
            if (smallestPosition.x > position.x) {
                smallestPosition.x = position.x;
            }
            if (smallestPosition.y > position.y) {
                smallestPosition.y = position.y;
            }
        }
    }
}

static void getBiggestPosition(std::deque<std::deque<Position>>& positionLists, Position& biggestPosition)
{
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
}

static void displayMap(std::deque<std::string>& map)
{
    for (const auto& line : map) {
        std::cout << line << std::endl;
    }
    // std::cout << "width = " << map.at(0).length() << std::endl;
    // std::cout << "height = " << map.size() << std::endl;
}

static void displayPositions(std::deque<std::deque<Position>>& positionLists)
{
    for (auto& positionList : positionLists) {
        std::string prefix;
        for (auto& position : positionList) {
            std::cout << prefix << "[" << position.x << ", " << position.y << "]";
            prefix = ", ";
        }
        std::cout << std::endl;
    }
}

static std::deque<std::string> createMap(std::deque<std::deque<Position>>& positionLists, Position& biggestPosition)
{
    std::string line(biggestPosition.x + 1, '.');
    std::deque<std::string> map(biggestPosition.y + 1, line);
    bool beacon = false;

    for (const auto& positionList : positionLists) {
        for (const auto& position : positionList) {
            map.at(position.y)[position.x] = (beacon ? 'B' : 'S');
            beacon = !beacon;
        }
    }

    return map;
}

static void fixPositions(std::deque<std::deque<Position>>& positionLists, Position& smallestPosition)
{
    for (auto& positionList : positionLists) {
        for (auto& position : positionList) {
            position.x -= smallestPosition.x;
            position.y -= smallestPosition.y;
        }
    }
}

static std::deque<int> initBeaconDistanceList(std::deque<std::deque<Position>>& positionLists)
{
    std::deque<int> beaconDistanceList(positionLists.size(), 0);

    for (int i = 0; i < positionLists.size(); ++i) {
        beaconDistanceList.at(i) = std::abs(positionLists.at(i).at(0).x - positionLists.at(i).at(1).x) + std::abs(positionLists.at(i).at(0).y - positionLists.at(i).at(1).y);
    }

    return beaconDistanceList;
}

static void expandBeaconTop();

static void expandBeaconRangeTopRight(std::deque<std::string>& map, Position& position, int beaconDistance)
{
    int x = position.x;
    int y = position.y;

    for (int i = beaconDistance; i >= 0; --i) {
        if (x >= map.at(0).length()) {
            break;
        }
        for (int j = i; j >= 0; --j) {
            if (y < 0) {
                break;
            }
            if (map.at(y)[x] == '.') {
                map.at(y)[x] = '#';
            }
            --y;
        }
        y = position.y;
        ++x;
    }
}

static void expandBeaconRangeBottomRight(std::deque<std::string>& map, Position& position, int beaconDistance)
{
    int x = position.x;
    int y = position.y;

    for (int i = beaconDistance; i >= 0; --i) {
        if (x >= map.at(0).length()) {
            break;
        }
        for (int j = i; j >= 0; --j) {
            if (y >= map.size()) {
                break;
            }
            if (map.at(y)[x] == '.') {
                map.at(y)[x] = '#';
            }
            ++y;
        }
        y = position.y;
        ++x;
    }
}

static void expandBeaconRangeBottomLeft(std::deque<std::string>& map, Position& position, int beaconDistance)
{
    int x = position.x;
    int y = position.y;

    for (int i = beaconDistance; i >= 0; --i) {
        if (x < 0) {
            break;
        }
        for (int j = i; j >= 0; --j) {
            if (y >= map.size()) {
                break;
            }
            if (map.at(y)[x] == '.') {
                map.at(y)[x] = '#';
            }
            ++y;
        }
        y = position.y;
        --x;
    }
}

static void expandBeaconRangeTopLeft(std::deque<std::string>& map, Position& position, int beaconDistance)
{
    int x = position.x;
    int y = position.y;

    for (int i = beaconDistance; i >= 0; --i) {
        if (x < 0) {
            break;
        }
        for (int j = i; j >= 0; --j) {
            if (y < 0) {
                break;
            }
            if (map.at(y)[x] == '.') {
                map.at(y)[x] = '#';
            }
            --y;
        }
        y = position.y;
        --x;
    }
}

static void expandAllBeaconRange(std::deque<std::string>& map, std::deque<std::deque<Position>>& positionLists, std::deque<int>& beaconDistanceList)
{
    for (int i = 0; i < beaconDistanceList.size(); ++i) {
        expandBeaconRangeTopRight(map, positionLists.at(i).at(0), beaconDistanceList.at(i));
        expandBeaconRangeBottomRight(map, positionLists.at(i).at(0), beaconDistanceList.at(i));
        expandBeaconRangeBottomLeft(map, positionLists.at(i).at(0), beaconDistanceList.at(i));
        expandBeaconRangeTopLeft(map, positionLists.at(i).at(0), beaconDistanceList.at(i));
    }
}

static bool isOccupied(Position& position, std::deque<std::deque<Position>>& positionLists)
{
    for (const auto& positionList : positionLists) {
        for (const auto& busyPosition : positionList) {
            if (position.x == busyPosition.x and position.y == busyPosition.y) {
                std::cout << "NIQUE ZEUBI" << std::endl;
                return true;
            }
        }
    }
    return false;
}

static bool inBeaconRange(Position& position, std::deque<std::deque<Position>>& positionLists, std::deque<int>& beaconDistanceList)
{
    int distanceFromBeacon = 0;

    for (int i = 0; i < positionLists.size(); ++i) {
        distanceFromBeacon = std::abs(position.x - positionLists.at(i).at(0).x) + std::abs(position.y - positionLists.at(i).at(0).y);
        if (distanceFromBeacon <= beaconDistanceList.at(i)) {
            return true;
        }
    }
    return false;
}

static bool isBorderRange(Position& position, std::deque<std::deque<Position>>& positionLists, std::deque<int>& beaconDistanceList)
{
    int distanceFromBeacon = 0;

    for (int i = 0; i < positionLists.size(); ++i) {
        distanceFromBeacon = std::abs(position.x - positionLists.at(i).at(0).x) + std::abs(position.y - positionLists.at(i).at(0).y);
        if (distanceFromBeacon == beaconDistanceList.at(i) + 1) {
            return true;
        }
    }
    return false;
}

static void displayDistance(std::deque<int>& beaconDistanceList)
{
    for (const auto& beaconDistance : beaconDistanceList) {
        std::cout << beaconDistance << std::endl;
    }
}

static int getBiggestBeaconDistance(std::deque<int>& beaconDistanceList)
{
    int biggestBeaconDistance = 0;

    for (const auto& beaconDistance : beaconDistanceList) {
        if (biggestBeaconDistance < beaconDistance) {
            biggestBeaconDistance = beaconDistance;
        }
    }

    return biggestBeaconDistance;
}

bool checkPositionInGivenDirection(std::deque<std::deque<Position>>& positionLists, std::deque<int>& beaconDistanceList, int i, Position& position, int directionX, int directionY, int distanceX, int distanceY)
{
    position.x = positionLists.at(i).at(0).x + (distanceX * directionX);
    position.y = positionLists.at(i).at(0).y + (distanceY * directionY);

    return position.x >= 0 and position.x <= 4000000 and position.y >= 0 and position.y <= 4000000 and not inBeaconRange(position, positionLists, beaconDistanceList);
}

void findDistressBeacon(std::deque<std::deque<Position>>& positionLists, std::deque<int>& beaconDistanceList, Position& position)
{
    int directionX = 0;
    int directionY = 0;
    int distanceY = 0;

    for (int i = 0; i < positionLists.size(); ++i) {
        for (int distanceX = 0; distanceX < beaconDistanceList.at(i); ++distanceX) {

            int distanceY = beaconDistanceList.at(i) + 1 - distanceX;

            if (checkPositionInGivenDirection(positionLists, beaconDistanceList, i, position, 1, -1, distanceX, distanceY) or checkPositionInGivenDirection(positionLists, beaconDistanceList, i, position, -1, 1, distanceX, distanceY) or checkPositionInGivenDirection(positionLists, beaconDistanceList, i, position, -1, -1, distanceX, distanceY) or checkPositionInGivenDirection(positionLists, beaconDistanceList, i, position, 1, 1, distanceX, distanceY)) {
                return;
            }
        }
    }
}

void beaconExclusionZone(std::ifstream& fileContent)
{
    std::deque<std::string> lines;
    std::deque<std::deque<Position>> positionLists;
    std::deque<int> beaconDistanceList;
    Position position = {.x = 0, .y = 0};

    getInputFromStdin(fileContent, lines);
    parseInput(lines, positionLists);

    beaconDistanceList = initBeaconDistanceList(positionLists);

    findDistressBeacon(positionLists, beaconDistanceList, position);

    std::cout << "solution = [" << position.x << ", " << position.y << "]" << std::endl;
}

// void beaconExclusionZone(std::ifstream& fileContent)
// {
//     std::deque<std::string> lines;
//     std::deque<std::deque<Position>> positionLists;
//     std::deque<int> beaconDistanceList;
//     // std::deque<std::string> map;
//     Position position = {.x = 0, .y = 2000000};
//     Position smallestPosition = {.x = 0, .y = 0};
//     Position biggestPosition = {.x = 0, .y = 0};
//     int biggestBeaconRadius = 0;
//     int noBeacon = 0;
//
//     getInputFromStdin(fileContent, lines);
//     parseInput(lines, positionLists);
//
//     displayPositions(positionLists);
//
//     getSmallestPosition(positionLists, smallestPosition);
//     // fixPositions(positionLists, smallestPosition);
//     getBiggestPosition(positionLists, biggestPosition);
//     beaconDistanceList = initBeaconDistanceList(positionLists);
//     biggestBeaconRadius = getBiggestBeaconDistance(beaconDistanceList);
//
//     position.x = smallestPosition.x - biggestBeaconRadius;
//
//     displayDistance(beaconDistanceList);
//
//     int mapWidth = std::abs(biggestPosition.x) + std::abs(smallestPosition.x) + (biggestBeaconRadius * 2);
//
//     for (; position.x < mapWidth; ++position.x) {
//         if (inBeaconRange(position, positionLists, beaconDistanceList)) {
//             ++noBeacon;
//         }
//     }
//
//     std::cout << "nb beacons in range -> " << noBeacon << std::endl;
//
//     // map = createMap(positionLists, biggestPosition);
//     //
//     // displayMap(map);
//     //
//     // std::cout << std::endl;
//
//     // expandAllBeaconRange(map, positionLists, beaconDistanceList);
//     //
//     // displayMap(map);
//     //
//     // std::cout << std::endl;
//     //
//     // for (int i = 0; i < map.size(); ++i) {
//     //     if (i == 10) {
//     //         std::cout << map.at(i) << std::endl;
//     //         std::cout << "Nb # -> " << std::count(map.at(i).begin(), map.at(i).end(), '#') << std::endl;
//     //     }
//     // }
// }
