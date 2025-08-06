#include "aoc.hpp"

#include <array>
#include <fstream>
#include <iostream>

bool displayHelp(const int ac, const char* const av[])
{
    bool helpDisplayed = false;
    if (ac == 1 or (ac == 2 and (strcmp(av[1], "-h") == 0 or (strcmp(av[1], "--help") == 0)))) {
        std::cout << "To display a selected day type `./aoc n` with `n` being the day number" << std::endl;
        helpDisplayed = true;
    }
    return helpDisplayed;
}

bool checkArgs(const int ac, const char* const av[])
{
    if (ac != 2) {
        std::cerr << "Invalid number of arguments" << std::endl;
        return false;
    }
    const int selectedDay = std::stoi(av[1]);
    if (selectedDay < 1 or selectedDay > 25) {
        std::cerr << "Invalid argument, you must enter a number between 0 and 25" << std::endl;
        return false;
    }
    return true;
}

std::string getContentFromFile(std::string& fileName)
{
    std::ifstream ifs(fileName);
    std::string content((std::istreambuf_iterator<char>(ifs)),
        (std::istreambuf_iterator<char>()));
    return content;
}

int main(const int ac, const char* av[])
{
    if (displayHelp(ac, av) or not checkArgs(ac, av)) {
        return 1;
    }

    const int selectedDay = std::stoi(av[1]) - 1;
    std::string puzzleInputPath = "day";
    puzzleInputPath += av[1];
    puzzleInputPath += "/puzzle_input.txt";

    std::ifstream fileContent(puzzleInputPath);

    std::array<std::function<void(std::ifstream & fileContent)>, 17>
        days = {&calorieCounting, &rockPaperScissors, &rucksackReorganization, &campCleanup, &supplyStacks, &tuningTrouble, &noSpaceLeftOnDevice, &treetopTreeHouse, &ropeBridge, &cathodeRayTube, &monkeyInTheMiddle, &hillClimbingAlgorithm, &distressSignal, &regolithReservoir, &beaconExclusionZone, &proboscideaVolcanium, &pyroclasticFlow};
    days.at(selectedDay)(fileContent);
    return 0;
}
