// ./src/binary/cpp/write_signed_chars.cpp

#include <fstream>
#include <iostream>
#include <cstdint>

int main() {
    const char* filename = "signed_chars.bin";
    std::ofstream out(filename, std::ios::binary);

    if (!out) {
        std::cerr << "Ошибка при открытии файла для записи.\n";
        return 1;
    }

    for (int i = 0; i <= 255; ++i) {
        uint8_t value = static_cast<uint8_t>(i);
        out.write(reinterpret_cast<const char*>(&value), sizeof(value));
    }

    out.close();
    return 0;
}

