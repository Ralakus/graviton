<p align=center><img src="./docs/logo.png" width=36%></p>

<h1 align=center>Graviton Programming language</h1>

[![crates.io](https://img.shields.io/crates/v/graviton?style=flat-square)](https://crates.io/crates/graviton)
[![license](https://img.shields.io/badge/license-MIT-blue.svg?style=flat-square)](./LICENSE)
[![build](https://img.shields.io/travis/Ralakus/graviton?style=flat-square)](https://travis-ci.org/Ralakus/graviton)
[![issues](https://img.shields.io/github/issues/Ralakus/graviton?style=flat-square)](https://github.com/Ralakus/graviton/issues)
[![repo size](https://img.shields.io/github/repo-size/Ralakus/graviton?style=flat-square)](https://github.com/Ralakus/graviton)
[![code size](https://img.shields.io/github/languages/code-size/Ralakus/graviton?style=flat-square)](https://github.com/Ralakus/graviton)

## Build requirements
* Rust beta with cargo

## How to build 
0. Run `cargo build`

## What is Graviton?
Graviton is a fast programming language that is minimal and simplistic with a simple and easy to read compiler. (Almost) Everything is an expression unless a semicolon is used `;` then it becomes a statement which gives a clear distinction between expressions and statements.

#### Notice
Graviton is still under heavy development so it is bound to have breaking changes.

## Current status
I've picked back up the project as of December 2023 to work on in my free time between semesters. I'm still evaluating the whole asynchronous compiler stages rebuild to see if I bit off more than I can chew in that aspect. I'm currently relearning the codebase and cleaning it up as I go to see if it's worth saving the current architecture or scrapping it in favor of a new one.

## Compiler design
Graviton's compiler is designed to be extremely efficient (TBD) and asynchronous (TBD). Each stage runs in parallel (TBD) rather than serial which speeds the compiler up greatly on multhreaded systems.

## How can follow the progress?
* There is a [Trello](https://trello.com/b/Z2PQHhgy/graviton) board set up for this project