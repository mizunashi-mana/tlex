#!/usr/bin/env bash

[ "$DEBUG" = "true" ] && set -x
set -euo pipefail

PROJECT_DIR=${PROJECT_DIR:-"$(cd "$(dirname "$(dirname "${BASH_SOURCE[0]}")")" && pwd)"}
CANDIDATE="${CANDIDATE:-"true"}"

cd "$PROJECT_DIR"

if [ -z "$*" ]; then
    echo "Some items is needed." >&2
    exit 1
fi

if [ "$(git rev-parse --abbrev-ref HEAD)" != "master" ]; then
    echo "Not on master branch." >&2
    exit 1
fi

if [ "$(git status --short | wc -l)" != "0" ]; then
    echo "Not staged changes are available." >&2
    exit 1
fi

git fetch origin master
if [ -z "${FORCE_RELEASE:-}" ]; then
    if [ \
        "$(git show --format=format:%H -s origin/master)" \
        != \
        "$(git show --format=format:%H -s master)" \
    ]; then
        cat >&2 <<EOS
You may forget to pull master changes.
If you are ok, please rerun with FORCE_RELEASE environment.
EOS
        exit 1
    fi
fi

for item in "$@"; do
    echo "Publishing $item"

    target="$(echo "$item" | sed 's/-[0-9.]*$//')"
    cabal sdist "$target"

    dist_file="$PROJECT_DIR/dist-newstyle/sdist/$item.tar.gz"
    if [ ! -e "$dist_file" ]; then
        echo "$item is not exists." >&2
        exit 1
    fi

    if [ "$CANDIDATE" = "true" ]; then
        cabal upload "$dist_file"
    else
        cabal upload --publish "$dist_file"
        git tag "$item"
        git push origin "$item"
    fi
done
