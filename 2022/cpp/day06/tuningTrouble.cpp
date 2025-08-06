#include <fstream>
#include <iostream>
#include <string>

bool checkStartOfPacket(std::string& dataStream, int i)
{
    std::string subString = dataStream.substr(i, 4);

    for (int i = 0; i < subString.length() - 1; ++i) {
        for (int j = i + 1; j < subString.length(); ++j) {
            if (subString[i] == subString[j]) {
                return false;
            }
        }
    }
    return true;
}

bool checkStartOfMessage(std::string& dataStream, int i)
{
    std::string subString = dataStream.substr(i, 14);

    for (int i = 0; i < subString.length() - 1; ++i) {
        for (int j = i + 1; j < subString.length(); ++j) {
            if (subString[i] == subString[j]) {
                return false;
            }
        }
    }
    return true;
}

void tuningTrouble(std::ifstream& fileContent)
{
    std::string dataStream;
    fileContent >> dataStream;

    std::cout << dataStream << std::endl;

    for (int i = 0; i < dataStream.length() - 14; ++i) {
        if (checkStartOfMessage(dataStream, i)) {
            std::cout << dataStream.substr(i, 14) << std::endl;
            std::cout << i + 14 << std::endl;
            return;
        }
    }
}
