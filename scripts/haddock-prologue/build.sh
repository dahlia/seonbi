#!/bin/bash
# Prerequisites:
# - Pandoc 2.0+
# - yq
# - Haskell Stack
# - GNU sed
set -e
root="$(dirname "$0")/../.."
package="$root/package.yaml"
readme="$root/README.md"
pandoc_script="$(dirname "$0")/omit-rich-elements.lua"
description="$(pandoc --lua-filter "$pandoc_script" -t haddock "$readme")"
backup="$(mktemp)"
cp "$package" "$backup"
cwd="$(pwd)"
exit_code=1
{
  yq \
    -y \
    --arg description "$description" \
    '.description = $description' \
    "$backup" > "$package"
  cd "$root"
  stack haddock --no-haddock-deps
  cd "$(stack path --dist-dir)/doc/html/"
  hackage_url='https://hackage.haskell.org/package/\1/docs/'
  sed -i -E \
    's|\.\./(([A-Za-z][[:alnum:]]*-)+[0-9]+(\.[0-9]+)*)/|'"$hackage_url|g" \
    ./*/*.html
  exit_code=0
} || true
cd "$cwd"
cp "$backup" "$package"
exit "$exit_code"
