#!/usr/bin/env bash
set -euf -o pipefail

cd "$(git rev-parse --show-toplevel)"

echo "Formatting Haskell code with fourmolu..."
git ls-files *.hs \
  | xargs --max-procs=0 -I {} fourmolu --quiet --mode inplace "{}"

echo "Formatting Cabal files with cabal-gild..."
git ls-files *.cabal cabal.project \
  | xargs --max-procs=0 -I {} cabal-gild -i "{}" -o "{}"
