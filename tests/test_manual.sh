dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$dir" || exit
nasm -f elf ./test.asm -o ./test.o && ld -m elf_i386 -s -o ./test ./test.o && rm ./test.o
./test
echo $?