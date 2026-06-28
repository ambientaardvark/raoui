#!/bin/bash
# Driver for the seatbelt spike: compiles seatbelt_test.c, runs each op in its
# own sandboxed process, and checks the outcome against what run_r's threat
# model requires. A clean denial is errno-based (the C probe prints BLOCKED); a
# violation that SIGKILLs instead shows up here as "KILLED" (exit > 128), which
# we flag because run_r could not capture such a failure as text.
set -u

here="$(cd "$(dirname "$0")" && pwd)"
bin="/tmp/seatbelt_test"
scratch="/private/tmp/raoui-sandbox"

mkdir -p "$scratch"
cc -o "$bin" "$here/seatbelt_test.c" -Wno-deprecated-declarations || {
  echo "compile failed"; exit 1; }

# op:expected (OK = action should succeed, BLOCKED = action should be refused)
ops="compute:OK read:OK write_home:BLOCKED write_scratch:OK net:BLOCKED exec:BLOCKED"

printf "%-15s %-10s %-10s %s\n" "OP" "EXPECT" "VERDICT" "DETAIL"
printf -- "------------------------------------------------------------------------\n"

fails=0; total=0
for pair in $ops; do
  total=$((total+1))
  op="${pair%%:*}"; expect="${pair##*:}"
  out="$("$bin" "$op" 2>&1)"; status=$?

  if [ "$status" -gt 128 ]; then
    got="KILLED"; detail="signal $((status - 128)) — uncatchable by run_r"
  else
    case "$out" in
      *"result=OK"*)      got="OK" ;;
      *"result=BLOCKED"*) got="BLOCKED" ;;
      *)                  got="ERROR" ;;
    esac
    detail="${out#op=$op result=$got detail=}"
  fi

  if [ "$got" = "$expect" ]; then verdict="PASS"; else verdict="FAIL"; fails=$((fails+1)); fi
  printf "%-15s %-10s %-10s %s\n" "$op" "$expect" "$verdict" "$detail"
done

printf -- "------------------------------------------------------------------------\n"
rm -f "$scratch/probe.txt" "$HOME/raoui_seatbelt_probe.txt"
rmdir "$scratch" 2>/dev/null
if [ "$fails" -eq 0 ]; then
  echo "all $total checks passed — sandbox blocks writes/network/exec, allows compute/reads"
  exit 0
else
  echo "$fails check(s) FAILED — see table"
  exit 1
fi
