#!/bin/bash
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
  yq write --inplace "$package" description "$description"
  cd "$root"
  stack haddock
  exit_code=0
} || true
cd "$cwd"
cp "$backup" "$package"
exit "$exit_code"
