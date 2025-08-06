#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

int getDirSize(std::vector<std::string> lines, std::string& dirName)
{
    std::stringstream ss;
    std::string word;
    std::string expectedLine = "$ cd " + dirName;
    int size = 0;
    int subFolder = 0;
    char* pEnd{};

    for (auto it = lines.begin(); it != lines.end();) {
        if (*it == "dir " + dirName) {
            ++subFolder;
        }
        if (*it == expectedLine) {
            --subFolder;
            if (subFolder == 0) {
                it = lines.erase(it);
                it = lines.erase(it);
                break;
            }
        }
        it = lines.erase(it);
    }
    for (auto it = lines.begin(); it != lines.end();) {
        ss << *it;
        ss >> word;
        if (word == "$") {
            return size;
        } else if (word == "dir") {
            ss >> word;
            size += getDirSize(lines, word);
        } else {
            size += std::strtol(word.c_str(), &pEnd, 10);
        }
        ss.str(std::string());
        ss.clear();
        word.clear();
        it = lines.erase(it);
    }

    return size;
}

void removeLastDir(std::string& path)
{
    auto pos = path.find_last_of('/');
    if (pos == 0) {
        ++pos;
    }
    path.erase(pos);
}

void noSpaceLeftOnDevice(std::ifstream& fileContent)
{
    std::vector<std::string> lines = {};
    std::vector<std::string> paths = {};
    std::vector<int> sizes = {};
    std::stringstream ss;
    std::string word;
    std::string path;
    int totalSize = 0;
    int i = -1;

    for (std::string line; std::getline(fileContent, line);) {
        lines.emplace_back(line);
    }

    for (auto it = lines.begin(); it != lines.end();) {
        ss << *it;
        ss >> word;
        if (word == "$") {
            ss >> word;
            if (word == "cd") {
                ss >> word;
                if (word != "..") {
                    if (path.empty() or path == "/") {
                        path += word;
                    } else {
                        path += "/" + word;
                    }
                    paths.emplace_back(path);
                    sizes.emplace_back(0);
                    ++i;
                } else {
                    removeLastDir(path);
                }
            }
        } else if (word == "dir") {
            ss >> word;
            sizes.at(i) += getDirSize(lines, word);
        } else {
            sizes.at(i) += std::stoi(word.c_str());
        }
        ss.str(std::string());
        ss.clear();
        word.clear();
        it = lines.erase(it);
    }

    std::sort(sizes.begin(), sizes.end());

    std::string prefix;

    std::cout << "{";
    for (int size : sizes) {
        std::cout << prefix << size;
        prefix = ", ";
    }
    std::cout << "}" << std::endl;

    prefix = "";

    for (int size : sizes) {
        if (70000000 - *std::max_element(sizes.begin(), sizes.end()) + size >= 30000000) {
            std::cout << size << std::endl;
            break;
        }
    }

    // std::cout << "{";
    // for (int i = 0; i < sizes.size(); ++i) {
    //     std::cout << prefix << "'" << paths.at(i) << "': " << sizes.at(i);
    //     prefix = ", ";
    // }
    // std::cout << "}" << std::endl;

    // for (int size : sizes) {
    //     std::cout << prefix << size;
    //     prefix = ", ";
    // }
    //
    // prefix = "";
    //
    // std::cout << "[";
    // for (auto& path : paths) {
    //     std::cout << prefix << path;
    //     prefix = ", ";
    // }
    // std::cout << "]" << std::endl;

    // std::cout << "System space \t: \t" << 70000000 << std::endl;
    // std::cout << "Used space \t: \t" << 70000000 - sizes.at(sizes.size() - 1) << std::endl;
    // std::cout << "Space needed \t: \t" << 30000000 << std::endl;
}
