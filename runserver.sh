#!/bin/bash
set -e

if [ "$(uname -s)" = "Darwin" ]; then
  ga_os=macos
  jq_plat=osx-amd64
else
  ga_os=ubuntu
  jq_plat=linux64
fi

if [[ ! -f seonbi-api ]]; then
  if [[ ! -f jq ]]; then
    wget -O jq "https://github.com/stedolan/jq/releases/download/jq-1.6/jq-$jq_plat"
    chmod +x jq
  fi
  auth="Authorization: token $GITHUB_TOKEN"
  artifacts_url="$(curl -v --header "$auth" \
                     https://api.github.com/repos/dahlia/seonbi/actions/workflows/25075/runs \
                  | ./jq -r '.workflow_runs[0].artifacts_url')"
  download_url="$(curl -v --header "$auth" "$artifacts_url" \
                 | ./jq -r --arg ga_os "$ga_os" \
                     '.artifacts | map(select(.name | startswith("binaries-" + $ga_os + "-"))) | sort_by(.name) | .[-1].archive_download_url')"
  wget \
    --content-disposition \
    --trust-server-names \
    --header "$auth" \
    "$download_url"
  unzip binaries-$ga_os-*.zip
  rm binaries-$ga_os-*.zip
  chmod +x seonbi seonbi-*
fi

./seonbi-api -p "$1" -o '*'
