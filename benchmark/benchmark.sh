#!/usr/bin/env bash

cargo run benchmark/fib.grav -e=exe -o=benchmark/fib
clang benchmark/fib.c -o benchmark/cfib
clang benchmark/fib.c -Ofast -o benchmark/cfibo

hyperfine -w 1000 -r 1000  "echo 14 | benchmark/fib" "echo 14 | python3 benchmark/fib.py" "echo 14 | lua benchmark/fib.lua" "echo 14 | luajit benchmark/fib.lua" "echo 14 | benchmark/cfib" "echo 14 | benchmark/cfibo"

rm benchmark/fib benchmark/cfib benchmark/cfibo