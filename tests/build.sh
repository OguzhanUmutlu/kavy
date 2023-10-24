dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$dir" || exit
tsc --noEmit ../src/index.ts
echo "Created index.js"
npx pkg ../src/