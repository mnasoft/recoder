#include <iostream>
#include <fstream>
#include <random>
#include <limits>

int main() {
    std::ofstream outFile("output.bin", std::ios::binary);
    std::ofstream txtFile("output.txt");
    
    if (!outFile || !txtFile) {
        std::cerr << "Ошибка открытия файлов!" << std::endl;
        return 1;
    }

    std::random_device rd;
    std::mt19937 gen(rd());

    auto writeRandomValues = [&](auto dist, auto& binOut, auto& txtOut, const std::string& typeName, bool convertToInt = false) {
        txtOut << typeName << ": ";
        for (int i = 0; i < 5; ++i) {
            auto value = dist(gen);
            binOut.write(reinterpret_cast<char*>(&value), sizeof(value));

            if (convertToInt) {
                txtOut << static_cast<int>(value) << " ";
            } else {
                txtOut << value << " ";
            }
        }
        txtOut << std::endl;
    };

    writeRandomValues(std::uniform_int_distribution<char>(std::numeric_limits<char>::min(), std::numeric_limits<char>::max()), outFile, txtFile, "char", true);
    writeRandomValues(std::uniform_int_distribution<unsigned char>(0, std::numeric_limits<unsigned char>::max()), outFile, txtFile, "unsigned char", true);
    writeRandomValues(std::uniform_int_distribution<short>(std::numeric_limits<short>::min(), std::numeric_limits<short>::max()), outFile, txtFile, "short");
    writeRandomValues(std::uniform_int_distribution<unsigned short>(0, std::numeric_limits<unsigned short>::max()), outFile, txtFile, "unsigned short");
    writeRandomValues(std::uniform_int_distribution<int>(std::numeric_limits<int>::min(), std::numeric_limits<int>::max()), outFile, txtFile, "int");
    writeRandomValues(std::uniform_int_distribution<unsigned int>(0, std::numeric_limits<unsigned int>::max()), outFile, txtFile, "unsigned int");
    writeRandomValues(std::uniform_int_distribution<long>(std::numeric_limits<long>::min(), std::numeric_limits<long>::max()), outFile, txtFile, "long");
    writeRandomValues(std::uniform_int_distribution<unsigned long>(0, std::numeric_limits<unsigned long>::max()), outFile, txtFile, "unsigned long");

    outFile.close();
    txtFile.close();

    std::cout << "Данные успешно записаны в output.bin и output.txt" << std::endl;

    return 0;
}
