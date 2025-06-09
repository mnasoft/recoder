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

    auto writeRandomValues = [&](auto dist, auto& binOut, auto& txtOut, const std::string& typeName) {
        txtOut << typeName << ": ";
        for (int i = 0; i < 5; ++i) {
            auto value = dist(gen);
            binOut.write(reinterpret_cast<char*>(&value), sizeof(value));
            txtOut << value << " ";
        }
        txtOut << std::endl;
    };

    writeRandomValues(std::uniform_real_distribution<float>(std::numeric_limits<float>::min(), std::numeric_limits<float>::max()), outFile, txtFile, "float");
    writeRandomValues(std::uniform_real_distribution<double>(std::numeric_limits<double>::min(), std::numeric_limits<double>::max()), outFile, txtFile, "double");
    writeRandomValues(std::uniform_real_distribution<long double>(std::numeric_limits<long double>::min(), std::numeric_limits<long double>::max()), outFile, txtFile, "long double");

    outFile.close();
    txtFile.close();

    std::cout << "Данные успешно записаны в output.bin и output.txt" << std::endl;

    return 0;
}
