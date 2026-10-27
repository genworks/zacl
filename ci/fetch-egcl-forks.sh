#!/bin/sh
# Clone the repositories listed in ci/egcl-forks.txt, each at its pinned
# commit, under deps/ (the deps job of .gitlab-ci.yml).  A shallow fetch
# of the one commit, so nothing moves under the pipeline when the forks do.
set -eu
root="${CI_PROJECT_DIR:-$(pwd)}"
grep -v '^#' "$root/ci/egcl-forks.txt" | while read -r dir url commit; do
    [ -n "$dir" ] || continue
    dest="$root/deps/$dir"
    mkdir -p "$dest"
    git -C "$dest" init -q
    git -C "$dest" fetch -q --depth 1 "$url" "$commit"
    git -C "$dest" checkout -q FETCH_HEAD
    echo "$dir @ $(git -C "$dest" rev-parse --short HEAD)"
done
