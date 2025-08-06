#include <fstream>
#include <iostream>
#include <string>
#include <vector>

bool isVisibleLeft(std::vector<std::string>& trees, int i, int j)
{
    char tree = trees.at(i)[j];

    for (; j > 0; --j) {
        if (tree <= trees.at(i)[j - 1]) {
            return false;
        }
    }
    return true;
}

bool isVisibleDown(std::vector<std::string>& trees, int i, int j)
{
    char tree = trees.at(i)[j];

    for (; i < trees.size() - 1; ++i) {
        if (tree <= trees.at(i + 1)[j]) {
            return false;
        }
    }
    return true;
}

bool isVisibleRight(std::vector<std::string>& trees, int i, int j)
{
    char tree = trees.at(i)[j];

    for (; j < trees.at(i).length() - 1; ++j) {
        if (tree <= trees.at(i)[j + 1]) {
            return false;
        }
    }
    return true;
}

bool isVisibleUp(std::vector<std::string>& trees, int i, int j)
{
    char tree = trees.at(i)[j];
    for (; i > 0; --i) {
        if (tree <= trees.at(i - 1)[j]) {
            return false;
        }
    }
    return true;
}

bool isTreeVisible(std::vector<std::string>& trees, int i, int j)
{
    // std::cout << "======" << std::endl;
    // std::cout << "isVisibleUp : " << isVisibleUp(trees, i, j) << std::endl;
    // std::cout << "isVisibleRight : " << isVisibleRight(trees, i, j) << std::endl;
    // std::cout << "isVisibleDown : " << isVisibleDown(trees, i, j) << std::endl;
    // std::cout << "isVisibleLeft : " << isVisibleLeft(trees, i, j) << std::endl;
    return isVisibleUp(trees, i, j) or isVisibleRight(trees, i, j) or isVisibleDown(trees, i, j) or isVisibleLeft(trees, i, j);
}

int getNbVisibleLeft(std::vector<std::string>& trees, int i, int j)
{
    int score = 1;
    char tree = trees.at(i)[j];

    for (; j > 1; --j) {
        if (tree <= trees.at(i)[j - 1]) {
            return score;
        }
        ++score;
    }
    return score;
}

int getNbVisibleDown(std::vector<std::string>& trees, int i, int j)
{
    int score = 1;
    char tree = trees.at(i)[j];

    for (; i < trees.size() - 2; ++i) {
        if (tree <= trees.at(i + 1)[j]) {
            return score;
        }
        ++score;
    }
    return score;
}

int getNbVisibleRight(std::vector<std::string>& trees, int i, int j)
{
    int score = 1;
    char tree = trees.at(i)[j];

    for (; j < trees.at(i).length() - 2; ++j) {
        if (tree <= trees.at(i)[j + 1]) {
            return score;
        }
        ++score;
    }
    return score;
}

int getNbVisibleUp(std::vector<std::string>& trees, int i, int j)
{
    int score = 1;
    char tree = trees.at(i)[j];

    for (; i > 1; --i) {
        if (tree <= trees.at(i - 1)[j]) {
            return score;
        }
        ++score;
    }
    return score;
}

int getNbVisible(std::vector<std::string>& trees, int i, int j)
{
    int upScore = getNbVisibleUp(trees, i, j);
    int leftScore = getNbVisibleLeft(trees, i, j);
    int downScore = getNbVisibleDown(trees, i, j);
    int rightScore = getNbVisibleRight(trees, i, j);
    return upScore * rightScore * downScore * leftScore;
}

void treetopTreeHouse(std::ifstream& fileContent)
{
    std::vector<std::string> trees = {};
    int nbVisible = 0;
    int mostVisible = 0;

    for (std::string line; std::getline(fileContent, line);) {
        trees.emplace_back(line);
    }

    for (int i = 0; i < trees.size(); ++i) {
        for (int j = 0; j < trees.at(i).length(); ++j) {
            nbVisible = getNbVisible(trees, i, j);
            if (nbVisible > mostVisible) {
                mostVisible = nbVisible;
            }
        }
    }
    std::cout << "Visible trees : " << mostVisible << std::endl;
}
