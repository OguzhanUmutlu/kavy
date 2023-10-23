dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$dir/../" || exit
rm -r build
mkdir build
cd build || exit

bun --bun run ../src/index.ts || exit

nasm -f elf out.asm -o out.o || exit
ld -m elf_i386 -s -o out out.o || exit
./out
echo $?
