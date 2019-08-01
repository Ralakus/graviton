<img src="./docs/logo.png" width=25%>

# Graviton Programming language

[![crates.io](https://img.shields.io/crates/v/graviton?style=flat-square)](https://crates.io/crates/graviton)
[![license](https://img.shields.io/badge/license-MIT-blue.svg?style=flat-square)](./LICENSE)
[![build](https://img.shields.io/travis/Ralakus/graviton?style=flat-square)](https://travis-ci.org/Ralakus/graviton)
[![issues](https://img.shields.io/github/issues/Ralakus/graviton?style=flat-square)](https://github.com/Ralakus/graviton/issues)
[![repo size](https://img.shields.io/github/repo-size/Ralakus/graviton?style=flat-square)](https://github.com/Ralakus/graviton)
[![code size](https://img.shields.io/github/languages/code-size/Ralakus/graviton?style=flat-square)](https://github.com/Ralakus/graviton)

## Build requirements
* Rust nightly with cargo

## How to build 
0. Run `cargo build`

## What is Graviton?
Graviton is a fast programming language that is minimal and simplistic with a simple and easy to read compiler. Everything is an expression unless a semicolon is used `;` then it becomes a statement which gives a clear distinction between expressions and statements.  
Graviton is still under heavy development so it is bound to have breaking changes

## Examples
[Iterative fibonacci example](./examples/fib.grav) 
```rust
let output = if (let fib_number = {
        let n = read_num();

        let mut prevprevn = 0;
        let mut prevn = 0;
        let mut curn = 1;

        let mut i = 2;

        while i <= n {

            prevprevn = prevn;

            prevn = curn;

            curn = prevprevn + prevn;

            i = i + 1;

        };

        return curn;
    }) != 377 {
        fib_number
    } else {
        -fib_number
    };

println(output);
```

## How can follow the progress?
* Follow it on Discord! There are also some other cool projects on this Discord like [Wolf](https://github.com/Ralakus/wolf-lang), FlukeWM, ModEngine, Lir Language, and more https://discord.gg/RmgjcES