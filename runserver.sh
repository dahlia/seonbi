#!/bin/bash
set -e
if [[ ! -f seonbi-api ]]; then
  if [[ ! -f jq-linux64 ]]; then
    wget https://github.com/stedolan/jq/releases/download/jq-1.6/jq-linux64
    chmod +x jq-linux64
  fi
  version="$(curl -v https://api.github.com/repos/dahlia/seonbi/releases/latest | ./jq-linux64 -r .tag_name)"
  wget "https://github.com/dahlia/seonbi/releases/download/$version/seonbi-$version.linux-x86_64.tar.bz2"
  tar xvfj "seonbi-$version.linux-x86_64.tar.bz2"
fi
./seonbi-api -p "$1" -o '*'
