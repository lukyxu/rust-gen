#!/bin/bash
mrustc -L $MRUSTC_STD_PATH base.rs 2>&1 >/dev/null
if [ $? != 0 ]; then
    exit 1
fi

rustc base.rs -o base-2 2>&1 >/dev/null
if [ $? != 0 ]; then
    exit 1
fi

./base-2
if [ $? != 0 ]; then
    exit 1
fi

./base
if [ $? == 136 ]; then
    exit 0
fi

exit 1