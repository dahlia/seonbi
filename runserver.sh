#!/bin/bash
set -evx

if [ "$(uname -s)" = "Darwin" ]; then
  os=macos-x86_64
else
  os=linux-x86_64
fi

download_url="https://dahlia.github.io/seonbi/dists/latest/seonbi.$os.tar.bz2"

if [[ ! -f seonbi-api ]]; then
  wget "$download_url"
  name="$(basename "$download_url")"
  tar xvfj "$name"
  rm "$name"
  chmod +x seonbi seonbi-*
fi

./seonbi-api -p "$1" -o '*'
