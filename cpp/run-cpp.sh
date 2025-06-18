#!/bin/bash

# Проверка наличия аргумента
if [ $# -eq 0 ]; then
    echo "Использование: $0 <файл.cpp>"
    exit 1
fi

# Получение имени файла и базового имени
SOURCE_FILE="$1"
BASE_NAME="${SOURCE_FILE%.cpp}"
EXE_NAME="${BASE_NAME}.exe"

# Компиляция
g++ -o "${EXE_NAME}" "$SOURCE_FILE"

# Проверка успешности компиляции
if [ $? -eq 0 ]; then
    echo "Компиляция прошла успешно. Запуск $BASE_NAME:"
    echo "--------------------------------------------"
    ./"${EXE_NAME}"
else
    echo "Ошибка компиляции."
    exit 2
fi
