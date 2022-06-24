#!/bin/bash
set -e

OUTPUT_1=$(rustc -A warnings base-manual.rs -o output_1 2>&1 >/dev/null)
OUTPUT_2=$(mrustc -L $MRUSTC_STD_PATH base-manual.rs -o output_2 2>&1 >/dev/null)
O1=$(./output_1)
O2=$(./output_2)

if [ "$O1" != "$O2" ];
then
exit 0
fi
exit 1