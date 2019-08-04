#!/usr/bin/env sh

cargo run test/1.grav
cargo run test/2.grav
cargo run test/3.grav
cargo run test/4.grav -e=ast --no_run
cargo run test/5.grav -e=ast --no_run
cargo run test/fn.grav -e=ast --no_run
echo "14" | cargo run examples/fib.grav