#!/bin/bash
set -e

cd "$(dirname "$0")/.."

echo "Building..."
dune build profiling/bench.exe

echo "Running perf record..."
perf record -g --call-graph dwarf ./_build/default/profiling/bench.exe

echo "Generating flame graph..."
# Requires: https://github.com/brendangregg/FlameGraph
perf script | stackcollapse-perf.pl | flamegraph.pl > profiling/flame.svg

echo "Done! Open profiling/flame.svg in a browser"
