#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

// number meaning:
//     -1 -> opening bracket
//     -2 -> closing bracket
//     * >= 0 -> an number

using Packet = int;

struct Packets {
    std::string packetString;
    std::vector<Packet> packets{};
};

enum PacketRetValue {
    FIRST_BIGGER = (-1),
    SECOND_BIGGER = 1,
    EQUAL = 0
};

void initPacket(std::string& line, std::vector<Packet>& packets)
{
    for (int i = 0; line[i] != '\0'; ++i) {
        if (line[i] == '[') {
            packets.emplace_back(-1);
        } else if (isdigit(line[i]) != 0) {
            packets.emplace_back(std::stoi(&line[i]));
        } else if (line[i] == ']') {
            packets.emplace_back(-2);
        }
    }
}

PacketRetValue compareIntWithList(int firstNumber, std::vector<Packet>& secondPackets, int& j)
{
    int secondNumber = secondPackets.at(j);

    if (secondNumber == -1) {
        return (compareIntWithList(firstNumber, secondPackets, ++j));
    } else if (secondNumber == -2 or firstNumber > secondNumber) {
        return FIRST_BIGGER;
    } else if (firstNumber < secondNumber or secondPackets.at(j + 1) >= -1) {
        return SECOND_BIGGER;
    }
    ++j;
    return EQUAL;
}

PacketRetValue compareListWithInt(std::vector<Packet>& firstPackets, int secondNumber, int& i)
{
    int firstNumber = firstPackets.at(i);

    if (firstNumber == -1) {
        return (compareListWithInt(firstPackets, secondNumber, ++i));
    } else if (firstNumber == -2 or firstNumber < secondNumber) {
        return SECOND_BIGGER;
    } else if (firstNumber > secondNumber or firstPackets.at(i + 1) >= -1) {
        return FIRST_BIGGER;
    }
    ++i;
    return EQUAL;
}

PacketRetValue compareIntWithInt(int firstNumber, int secondNumber)
{
    if (firstNumber > secondNumber) {
        return FIRST_BIGGER;
    } else if (firstNumber < secondNumber) {
        return SECOND_BIGGER;
    }
    return EQUAL;
}

int comparePackets(std::vector<Packet>& firstPackets, std::vector<Packet>& secondPackets, int pairId)
{
    int firstNumber = 0;
    int secondNumber = 0;
    int comparaisonResult = 0;
    int i = 0;
    int j = 0;

    for (; i < firstPackets.size() and j < secondPackets.size(); ++i, ++j) {
        firstNumber = firstPackets.at(i);
        secondNumber = secondPackets.at(j);
        // std::cout << firstNumber << " and " << secondNumber << std::endl;
        // INT and INT
        if (firstNumber >= 0 and secondNumber >= 0) {
            comparaisonResult = compareIntWithInt(firstNumber, secondNumber);
        }
        // INT and [
        else if (firstNumber >= 0 and secondNumber == -1) {
            comparaisonResult = compareIntWithList(firstNumber, secondPackets, ++j);
        }
        // [ and INT
        else if (firstNumber == -1 and secondNumber >= 0) {
            comparaisonResult = compareListWithInt(firstPackets, secondNumber, ++i);
        }
        // INT and ] or [ and ]
        else if ((firstNumber >= 0 and secondNumber == -2) or (firstNumber == -1 and secondNumber == -2)) {
            comparaisonResult = FIRST_BIGGER;
        } // ] and INT or ] and [
        else if ((firstNumber == -2 and secondNumber >= 0) or (firstNumber == -2 and secondNumber == -1)) {
            comparaisonResult = SECOND_BIGGER;
        }
        // [ and [ or ] and ]
        else if ((firstNumber == -1 or secondNumber == -1) or (firstNumber == -2 or secondNumber == -2)) {
            comparaisonResult = EQUAL;
        }
        if (comparaisonResult == FIRST_BIGGER) {
            return 0;
        } else if (comparaisonResult == SECOND_BIGGER) {
            return pairId;
        }
    }
    return pairId;
}

// void displayPackets(std::vector<Packet>& packets)
// {
//     std::string prefix;
//     std::cout << "[";
//     for (auto& packet : packets) {
//         std::cout << prefix << packet;
//         prefix = ", ";
//     }
//     std::cout << "]" << std::endl;
// }

void addPacket(std::vector<Packets>& packets, std::string& line)
{
    Packets packet{.packetString = line};

    initPacket(line, packet.packets);

    packets.emplace_back(packet);
}

void displayAllPackets(std::vector<Packets>& packets)
{
    for (auto& packet : packets) {
        std::cout << "Packet string : " << std::endl;
        std::cout << packet.packetString << std::endl;
        std::cout << "Packet converted : " << std::endl;
        std::string prefix;
        std::cout << "[";
        for (auto& subPacket : packet.packets) {
            std::cout << prefix << subPacket;
            prefix = ", ";
        }
        std::cout << "]" << std::endl
                  << std::endl;
    }
}

void displayPackets(std::vector<Packets>& packets)
{
    for (auto& packet : packets) {
        std::cout << packet.packetString << std::endl;
    }
}

void initPackets(std::ifstream& fileContent, std::vector<Packets>& packets)
{
    for (std::string line; std::getline(fileContent, line);) {
        addPacket(packets, line);
        std::getline(fileContent, line);
        addPacket(packets, line);
        std::getline(fileContent, line);
    }
    std::string specialPacket = "[[2]]";
    addPacket(packets, specialPacket);
    specialPacket = "[[6]]";
    addPacket(packets, specialPacket);
}

bool arePacketsSorted(std::vector<Packets>& packets)
{
    for (int i = 0; i < packets.size() - 1; ++i) {
        if (comparePackets(packets.at(i).packets, packets.at(i + 1).packets, 1) == 0) {
            return false;
        }
    }
    return true;
}

void sortPackets(std::vector<Packets>& packets)
{
    Packets tmp = {};

    while (not arePacketsSorted(packets)) {
        for (int i = 0; i < packets.size() - 1;) {
            if (comparePackets(packets.at(i).packets, packets.at(i + 1).packets, 1) == 0) {
                tmp = packets.at(i);
                packets.at(i) = packets.at(i + 1);
                packets.at(i + 1) = tmp;
                i = 0;
            } else {
                ++i;
            }
        }
    }
}

bool samePacket(std::vector<Packet>& firstPacket, std::vector<Packet>& secondPacket)
{
    if (firstPacket.size() not_eq secondPacket.size()) {
        return false;
    }
    for (int i = 0; i < firstPacket.size(); ++i) {
        if (firstPacket.at(i) not_eq secondPacket.at(i)) {
            return false;
        }
    }
    return true;
}

int findPacket(std::vector<Packets>& packets, std::vector<Packet>& packetToFind)
{
    int index = 1;

    for (auto& packet : packets) {
        if (samePacket(packet.packets, packetToFind)) {
            return index;
        }
        ++index;
    }

    return -1;
}

void distressSignal(std::ifstream& fileContent)
{
    std::vector<Packets> packets;
    std::vector<Packet> firstSpecialPackets = {-1, -1, 2, -2, -2};
    std::vector<Packet> secondSpecialPackets = {-1, -1, 6, -2, -2};
    int firstSpecialPacket = 0;
    int secondSpecialPacket = 0;

    initPackets(fileContent, packets);
    sortPackets(packets);
    firstSpecialPacket = findPacket(packets, firstSpecialPackets);
    secondSpecialPacket = findPacket(packets, secondSpecialPackets);

    displayPackets(packets);
    std::cout << "firstSpecialPacket index : " << firstSpecialPacket << std::endl;
    std::cout << "secondSpecialPacket index : " << secondSpecialPacket << std::endl;
    std::cout << "result = " << firstSpecialPacket * secondSpecialPacket << std::endl;
}

// void distressSignal(std::ifstream& fileContent)
// {
//     std::stringstream ss;
//     std::string word;
//     std::vector<Packet> firstPackets;
//     std::vector<Packet> secondPackets;
//     int pairId = 1;
//     int validPackets = 0;
//
//     int result = 0;
//     std::string tmp;
//     for (std::string line; std::getline(fileContent, line);) {
//         initPacket(line, firstPackets);
//         tmp = line;
//         std::getline(fileContent, line);
//         initPacket(line, secondPackets);
//         result = comparePackets(firstPackets, secondPackets, pairId);
//         if (result != 0) {
//             std::cout << "== Pair " << pairId << " ==" << std::endl;
//             std::cout << tmp << std::endl
//                       << line << std::endl;
//             std::cout << "inputs are" << (result == 0 ? " NOT " : " ") << "in the right order" << std::endl
//                       << std::endl;
//         }
//         std::getline(fileContent, line);
//         validPackets += comparePackets(firstPackets, secondPackets, pairId);
//         firstPackets.clear();
//         secondPackets.clear();
//         ++pairId;
//     }
//     std::cout << "ValidPackets " << validPackets << std::endl;
// }
