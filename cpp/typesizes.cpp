// ./src/binary/cpp/typesizes.cpp

#include <iostream>
#include <climits>
#include <cfloat>
#include <cstdint>

int main() {
    std::cout << "Размеры и диапазоны числовых типов:\n\n";

    // Целочисленные типы
    std::cout << "char:\n";
    std::cout << "  size: " << sizeof(char) << " байт (" << CHAR_BIT * sizeof(char) << " бит)\n";
    std::cout << "  signed: от " << static_cast<int>(SCHAR_MIN) << " до " << static_cast<int>(SCHAR_MAX) << "\n";
    std::cout << "  unsigned: от 0 до " << UCHAR_MAX << "\n\n";

    std::cout << "short:\n";
    std::cout << "  size: " << sizeof(short) << " байт\n";
    std::cout << "  signed: от " << SHRT_MIN << " до " << SHRT_MAX << "\n";
    std::cout << "  unsigned: от 0 до " << USHRT_MAX << "\n\n";

    std::cout << "int:\n";
    std::cout << "  size: " << sizeof(int) << " байт\n";
    std::cout << "  signed: от " << INT_MIN << " до " << INT_MAX << "\n";
    std::cout << "  unsigned: от 0 до " << UINT_MAX << "\n\n";

    std::cout << "long:\n";
    std::cout << "  size: " << sizeof(long) << " байт\n";
    std::cout << "  signed: от " << LONG_MIN << " до " << LONG_MAX << "\n";
    std::cout << "  unsigned: от 0 до " << ULONG_MAX << "\n\n";

    std::cout << "long long:\n";
    std::cout << "  size: " << sizeof(long long) << " байт\n";
    std::cout << "  signed: от " << LLONG_MIN << " до " << LLONG_MAX << "\n";
    std::cout << "  unsigned: от 0 до " << ULLONG_MAX << "\n\n";

    // Логический тип
    std::cout << "bool:\n";
    std::cout << "  size: " << sizeof(bool) << " байт\n";
    std::cout << "  значения: false (0), true (1)\n\n";

    // Типы с плавающей точкой
    std::cout << "float:\n";
    std::cout << "  size: " << sizeof(float) << " байт\n";
    std::cout << "  диапазон: от " << FLT_MIN << " до " << FLT_MAX << "\n";
    std::cout << "  точность: " << FLT_DIG << " десятичных цифр\n\n";

    std::cout << "double:\n";
    std::cout << "  size: " << sizeof(double) << " байт\n";
    std::cout << "  диапазон: от " << DBL_MIN << " до " << DBL_MAX << "\n";
    std::cout << "  точность: " << DBL_DIG << " десятичных цифр\n\n";

    std::cout << "long double:\n";
    std::cout << "  size: " << sizeof(long double) << " байт\n";
    std::cout << "  диапазон: от " << LDBL_MIN << " до " << LDBL_MAX << "\n";
    std::cout << "  точность: " << LDBL_DIG << " десятичных цифр\n\n";

    // Указатели
    std::cout << "Размеры указателей:\n";
    std::cout << "  void*:         " << sizeof(void*) << " байт\n";
    std::cout << "  char*:         " << sizeof(char*) << " байт\n";
    std::cout << "  int*:          " << sizeof(int*) << " байт\n";
    std::cout << "  double*:       " << sizeof(double*) << " байт\n";
    std::cout << "  long double*:  " << sizeof(long double*) << " байт\n\n";

    // Типы из <cstdint>
    std::cout << "Типы из <cstdint>:\n";
    std::cout << "  int8_t:        " << sizeof(std::int8_t) << " байт\n";
    std::cout << "  uint8_t:       " << sizeof(std::uint8_t) << " байт\n";
    std::cout << "  int16_t:       " << sizeof(std::int16_t) << " байт\n";
    std::cout << "  uint16_t:      " << sizeof(std::uint16_t) << " байт\n";
    std::cout << "  int32_t:       " << sizeof(std::int32_t) << " байт\n";
    std::cout << "  uint32_t:      " << sizeof(std::uint32_t) << " байт\n";
    std::cout << "  int64_t:       " << sizeof(std::int64_t) << " байт\n";
    std::cout << "  uint64_t:      " << sizeof(std::uint64_t) << " байт\n";

    return 0;
}
