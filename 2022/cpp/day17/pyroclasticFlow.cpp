#include <array>
#include <deque>
#include <fstream>
#include <iostream>
#include <sstream>
#include <vector>

static void getInputFromStdin(std::ifstream& fileContent, std::deque<std::string>& lines)
{
    for (std::string line; std::getline(fileContent, line);) {
        lines.emplace_back(line);
    }
}

// Rock patterns

// ####
//
// .#.
// ###
// .#.
//
// ..#
// ..#
// ###
//
// #
// #
// #
// #
//
// ##
// ##

static void addPlaceNextRock(std::deque<std::string>& map)
{
    map.emplace_front(".......");
    map.emplace_front(".......");
    map.emplace_front(".......");
}

static void addNextRock(std::deque<std::string>& map, const std::vector<std::string>& rock)
{
    // NOLINTNEXTLINE
    for (auto it = rock.rbegin(); it not_eq rock.rend(); ++it) {
        map.emplace_front(*it);
    }
}

static bool rockStuck(std::deque<std::string>& map)
{
    for (int i = 0; i < map.size(); ++i) {
        for (int j = 0; j < 7; ++j) {
            if (map.at(i)[j] == '@' and (i == map.size() - 1 or map.at(i + 1)[j] == '#')) {
                return true;
            }
        }
    }
    return false;
}

static bool canRockMoveRight(std::deque<std::string>& map)
{
    for (const auto& line : map) {
        for (int j = 0; j < 7; ++j) {
            if (line[j] == '@' and (j == 6 or line[j + 1] == '#')) {
                return false;
            }
        }
    }
    return true;
}

static bool canRockMoveLeft(std::deque<std::string>& map)
{
    for (const auto& line : map) {
        for (int j = 0; j < 7; ++j) {
            if (line[j] == '@' and (j == 0 or line[j - 1] == '#')) {
                return false;
            }
        }
    }
    return true;
}

static void moveRockDown(std::deque<std::string>& map)
{
    for (int i = map.size() - 2; i >= 0; --i) {
        for (int j = 0; j < 7; ++j) {
            if (map.at(i)[j] == '@') {
                map.at(i + 1)[j] = '@';
                map.at(i)[j] = '.';
            }
        }
    }
    for (int j = 0; j < 7; ++j) {
        if (map.at(0)[j] == '#') {
            return;
        }
    }
    map.pop_front();
}

static void moveRockLeft(std::deque<std::string>& map)
{
    for (auto& line : map) {
        for (int j = 1; j < 7; ++j) {
            if (line[j] == '@') {
                line[j - 1] = '@';
                line[j] = '.';
            }
        }
    }
}

static void moveRockRight(std::deque<std::string>& map)
{
    for (auto& line : map) {
        for (int j = 5; j >= 0; --j) {
            if (line[j] == '@') {
                line[j + 1] = '@';
                line[j] = '.';
            }
        }
    }
}

static void moveRockSideways(std::deque<std::string>& map, char direction)
{
    if (direction == '>' and not canRockMoveRight(map) or direction == '<' and not canRockMoveLeft(map)) {
        return;
    }
    if (direction == '>') {
        moveRockRight(map);
    }
    if (direction == '<') {
        moveRockLeft(map);
    }
}

static void changeRockToStuck(std::deque<std::string>& map)
{
    for (auto& line : map) {
        for (int j = 0; j < 7; ++j) {
            if (line[j] == '@') {
                line[j] = '#';
            }
        }
    }
}

static int getTowerHeight(std::deque<std::string>& map)
{
    for (int i = 0; i < map.size(); ++i) {
        for (int j = 0; j < 7; ++j) {
            if (map.at(i)[j] == '#') {
                return (map.size() - i);
            }
        }
    }
    return -1;
}

void pyroclasticFlow(std::ifstream& fileContent)
{
    std::deque<std::string> lines;
    const std::vector<std::string> firstRock = {"..@@@@."};
    const std::vector<std::string> secondRock = {"...@...", "..@@@..", "...@..."};
    const std::vector<std::string> thirdRock = {"....@..", "....@..", "..@@@.."};
    const std::vector<std::string> fourthRock = {"..@....", "..@....", "..@....", "..@...."};
    const std::vector<std::string> fifthRock = {"..@@...", "..@@..."};
    const std::array<std::vector<std::string>, 5> rocks = {firstRock, secondRock, thirdRock, fourthRock, fifthRock};
    std::deque<std::string> map;
    int limit = 2022;
    int j = 0;

    getInputFromStdin(fileContent, lines);

    for (int i = 0; i < limit; ++i) {
        addPlaceNextRock(map);
        addNextRock(map, rocks.at(i % 5));
        moveRockSideways(map, lines.at(0).at(j % lines.at(0).length()));
        j++;
        while (not rockStuck(map)) {
            moveRockDown(map);
            moveRockSideways(map, lines.at(0).at(j % lines.at(0).length()));
            j++;
        }
        changeRockToStuck(map);
    }
    for (const auto& line : map) {
        std::cout << "|" << line << "|" << std::endl;
    }
    std::cout << "+-------+" << std::endl;

    std::cout << "The tower is " << getTowerHeight(map) << "m tall" << std::endl;
}
