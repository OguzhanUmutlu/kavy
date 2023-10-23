clear
bun --bun run ./index.ts
nasm -f elf out.asm -o out.o && ld -m elf_i386 -s -o out out.o && rm out.o
./out
echo $?
