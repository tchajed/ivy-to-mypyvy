# Ivy to mypyvy

[![CI](https://github.com/tchajed/ivy-to-mypyvy/actions/workflows/build.yml/badge.svg)](https://github.com/tchajed/ivy-to-mypyvy/actions/workflows/build.yml)

Convert an [Ivy](https://kenmcmil.github.io/ivy/) transition system (after its liveness-to-safety reduction) to a
[mypyvy](https://github.com/wilcoxjay/mypyvy) input file, as a first step towards applying invariant inference.

## Usage

To get an input file suitable for `ivy-to-mypyvy`, run `ivy_check l2s_debug=true
mutex.ivy > ivy.out`, then extract the relevant portion with

```
awk '/^after replace_named_binders/{ p = 1; next } /while \*/{ p = 0 } p' ivy.out | sed 's/;/;\n/g' > l2s.out
```

(Somebody perhaps the tool will do that for you.)

Run with `cargo run -- l2s.out`.

For debugging purposes, `cargo run -- --ivy l2s.out` will parse and then print
back the Ivy input.

## Development notes

We use [cargo-insta](https://crates.io/crates/cargo-insta) for snapshot testing,
which records the expected output in a file. This helps monitor changes in the
output while automatically managing the output.

Install with `cargo install cargo-insta` and then run `cargo insta test
--review`, which will prompt to accept changes if the output has changed. The
--tests still run as usual with `cargo test`.

The Ivy grammar is specified in [ivy.pest](src/ivy.pest), a [PEG
grammar](https://pest.rs/book/grammars/peg.html). PEGs are pretty cool.
