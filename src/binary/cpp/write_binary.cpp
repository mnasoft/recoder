// ./src/binary/cpp/write_binary.cpp


#include <iostream>
#include <fstream>
#include <vector>
#include <cstdint>
#include <sstream>


template <typename T>
void read_and_write(std::istream& in, std::ofstream& out, int count) {
    for (int i = 0; i < count; ++i) {
        T value;
        in >> value;
        T converted = static_cast<T>(value);
        out.write(reinterpret_cast<char*>(&converted), sizeof(T));
    }
}


int main() {
    std::ifstream input("sample_data.txt");
    std::ofstream output("output.bin", std::ios::binary);

    if (!input || !output) {
        std::cerr << "Ошибка открытия файлов.\n";
        return 1;
    }

    std::string line;
    while (std::getline(input, line)) {
        if (line.empty() || line[0] == '#') continue;

        std::istringstream iss(line);

        // Определяем, какой тип сейчас читаем
        static int section = 0;
        switch (section) {
            case 0: read_and_write<uint8_t>(iss, output, 10); break;
            case 1: read_and_write<uint16_t>(iss, output, 10); break;
            case 2: read_and_write<uint32_t>(iss, output, 10); break;
            case 3: read_and_write<uint64_t>(iss, output, 10); break;
            case 4: read_and_write<int8_t>(iss, output, 10); break;
            case 5: read_and_write<int16_t>(iss, output, 10); break;
            case 6: read_and_write<int32_t>(iss, output, 10); break;
            case 7: read_and_write<int64_t>(iss, output, 10); break;
            case 8: read_and_write<float>(iss, output, 10); break;
            case 9: read_and_write<double>(iss, output, 10); break;
            case 10: read_and_write<long double>(iss, output, 10); break;
        }
        ++section;
    }

    std::cout << "Данные успешно записаны в output.bin\n";
    return 0;
}
