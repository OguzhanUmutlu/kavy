dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$dir/../" || exit

bun --bun run ./src/index.ts ./tests/test.kavy -b || exit

./tests/test
echo $?
