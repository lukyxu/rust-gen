#!/bin/bash
set -e

OUTPUT_1=$(rustc -A warnings base.rs -o output_1 2>&1 >/dev/null)
OUTPUT_2=$(rustc +1.31.0 -C opt-level=z -A warnings base.rs -o output_2 2>&1 >/dev/null)

O1=$(./output_1)

set +e
O2=$(./output_2 2>&1 >/dev/null)
x="$?"
set -e
if [ $x == 101 ];
then
exit 0
fi
exit 1