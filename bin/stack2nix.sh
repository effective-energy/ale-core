#!/usr/bin/env nix-shell
#!nix-shell -p bash nix coreutils cabal-install cabal2nix nix-prefetch-git -i bash

nix_file="ale.nix"
nix_file_dir="build/nix"
root_rel="../.."

ale_root=$(dirname -- $(readlink -f -- "${BASH_SOURCE[0]}"))/../

mkdir -p "$ale_root/$nix_file_dir"
pushd "$ale_root/$nix_file_dir" > /dev/null
  stack2nix=$(nix-build -Q --no-out-link -A stack2nix "$root_rel")/bin/stack2nix

  $stack2nix --test "$@" "$root_rel" > "$nix_file".new
  mv "$nix_file".new "$nix_file"
popd > /dev/null
