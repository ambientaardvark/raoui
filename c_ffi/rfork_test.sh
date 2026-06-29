#!/bin/bash
# Driver for the R-fork spike: compiles rfork_test.c against the embedded R and
# runs it. Proves fork + sandbox + copy-on-write visibility of the parent's R
# session + parent integrity, all in one go.
set -u

here="$(cd "$(dirname "$0")" && pwd)"
bin="/tmp/rfork_test"
scratch="/private/tmp/raoui-sandbox"

R_HOME="$(R RHOME)"; export R_HOME
[ -n "$R_HOME" ] || { echo "could not determine R_HOME via 'R RHOME'"; exit 1; }
mkdir -p "$scratch"

cc -o "$bin" "$here/rfork_test.c" -Wno-deprecated-declarations || {
  echo "compile failed"; exit 1; }

rm -f "$HOME/rfork_leak.txt"
"$bin"; status=$?

rmdir "$scratch" 2>/dev/null
echo "---- driver: rfork_test exited $status ----"
exit "$status"
