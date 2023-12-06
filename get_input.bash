#!/usr/bin/env bash

set -euo pipefail

day=$1
URL=https://adventofcode.com/2023/day/$day/input
cookie=$(gpg -d cookie.gpg 2> /dev/null)

curl -O --cookie "session=$cookie" "$URL"

if [ ! $? ]; then
    echo "curl returned error"
fi
