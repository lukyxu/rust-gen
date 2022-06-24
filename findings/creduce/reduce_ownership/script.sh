#!/bin/bash
rustc -A warnings -o /dev/null ownership.rs |& exec grep -q "use of moved value"
