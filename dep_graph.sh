#!/usr/bin/env bash

cargo deps \
    --all-deps \
    --include-orphans \
    --subgraph tachyon tachyon_ast tachyon_backend tachyon_frontend \
    --subgraph-name "Tachyon" \
    | dot -Tpng > tachyon_deps.png 