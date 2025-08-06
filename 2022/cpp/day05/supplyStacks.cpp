#include <fstream>
#include <iostream>
#include <sstream>
#include <string>

static void getNumberFromLine(std::string& line, int& nbBox, int& fromBoxPile, int& toBoxPile)
{
    std::stringstream ss;
    ss << line;

    std::string tmp;

    ss >> tmp;
    ss >> tmp;
    std::stringstream(tmp) >> nbBox;
    ss >> tmp;
    ss >> tmp;
    std::stringstream(tmp) >> fromBoxPile;
    ss >> tmp;
    ss >> tmp;
    std::stringstream(tmp) >> toBoxPile;
}

void reverseString(std::string& string)
{
    int length = string.length();

    for (int i = 0; i < length / 2; i++) {
        std::swap(string[i], string[length - i - 1]);
    }
}

void initBoxPiles(std::ifstream& fileContent, std::string boxPiles[9])
{
    int j = 1;
    std::string line;
    std::getline(fileContent, line);

    for (; line[1] not_eq '1'; std::getline(fileContent, line)) {
        for (int i = 0; i < 9; ++i) {
            if (line[j] != ' ') {
                boxPiles[i].append(1, line[j]);
            }
            j += 4;
        }
        j = 1;
    }
    std::getline(fileContent, line);
    for (int i = 0; i < 9; ++i) {
        reverseString(boxPiles[i]);
    }
}

// void moveBoxesFromPileToPile(std::string boxPiles[9], int nbBox, int fromBoxPile, int toBoxPile)
// {
//     char c = 0;
//
//     for (int i = 0; i < nbBox; ++i) {
//         c = boxPiles[fromBoxPile].back();
//         boxPiles[fromBoxPile].pop_back();
//         boxPiles[toBoxPile].append(1, c);
//     }
// }

void moveBoxesFromPileToPile(std::string boxPiles[9], int nbBox, int fromBoxPile, int toBoxPile)
{
    char c = 0;

    boxPiles[toBoxPile].append(boxPiles[fromBoxPile].substr(boxPiles[fromBoxPile].length() - nbBox, nbBox));
    boxPiles[fromBoxPile].erase(boxPiles[fromBoxPile].length() - nbBox, nbBox);
}

void supplyStacks(std::ifstream& fileContent)
{
    std::string boxPiles[9];
    int nbBox = 0;
    int fromBoxPile = 0;
    int toBoxPile = 0;
    std::string result;
    std::string line;

    initBoxPiles(fileContent, boxPiles);
    while (std::getline(fileContent, line)) {
        getNumberFromLine(line, nbBox, fromBoxPile, toBoxPile);
        moveBoxesFromPileToPile(boxPiles, nbBox, fromBoxPile - 1, toBoxPile - 1);
    }
    for (const auto& boxPile : boxPiles) {
        result.append(1, boxPile.back());
    }
    std::cout << result << std::endl;
}
