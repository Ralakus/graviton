#!/usr/bin/env bash

cargo deps \
    --all-deps \
    --include-orphans \
    --subgraph graviton graviton_ast graviton_backend graviton_frontend \
    --subgraph-name "Graviton" \
    | dot -Tpng > graviton_deps.png 