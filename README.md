# Ivy to mypyvy

[![CI](https://github.com/tchajed/ivy-to-mypyvy/actions/workflows/build.yml/badge.svg)](https://github.com/tchajed/ivy-to-mypyvy/actions/workflows/build.yml)

Convert an [Ivy](https://kenmcmil.github.io/ivy/) transition system (after its liveness-to-safety reduction) to a
[mypyvy](https://github.com/wilcoxjay/mypyvy) input file, as a first step towards applying invariant inference.

## Usage

To get an input file suitable for `ivy-to-mypyvy`, run `ivy_check l2s_debug=true
mutex.ivy > l2s.out`.

Run with `cargo run -- l2s.out`. Currently for the `mutex` and `better_mutex` examples the
required cleanup is the following:

```sh
cargo run -- tests/mutex.l2s.out | sed -e 's/\?/thread/g' -e '2aimmutable constant t0: thread' > mutex.pyv
cargo run -- tests/better_mutex.l2s.out | sed -e 's/\?/thread/g' -e '2aimmutable constant t0: thread' > better_mutex.pyv
```

For `ticket.ivy`, we start with the example in the Ivy repo, then manually
simplify it (removing fancy Ivy features) to
[ticket.ivy](https://gist.github.com/tchajed/9222bb1195c93f8a799f2c028bca9039).
There is still some manual work which is more involved than above, so it's
wrapped up in a script: Run `./scripts/ticket.sh > ticket.pyv` to generate that
file.

The ticket example is tricky to support because in Ivy it uses actions with
return values, and after manually implementing that it's still tricky because
the liveness-to-safety reduction uses relations `l2s_d` and `l2s_a` over
arbitrary values rather than values from just one of the two sorts (thread and
ticket). In the other examples there's only one sort so this isn't an issue.

You can then verify the liveness property using the hand-written invariants, for
example:

```sh
mypyvy verify mutex.pyv
mypyvy verify better_mutex.pyv
mypyvy verify ticket.pyv
```

(I have an alias for `mypyvy`, without that you'd pass the path to
`mypyvy.sh`.)

For debugging purposes, `cargo run -- --ivy tests/mutex.l2s.out` will parse and then print
back the Ivy input.

## Development notes

We use [cargo-insta](https://crates.io/crates/cargo-insta) for snapshot testing,
which records the expected output in a file. This helps monitor changes in the
output while automatically managing the output.

Install with `cargo install cargo-insta` and then run `cargo insta test
--review`, which will prompt to accept changes if the output has changed. The
tests still run as usual with `cargo test`.

The Ivy grammar is written using rust-peg as an embedded DSL in
[ivy_l2s.rs](src/ivy_l2s.rs). The grammar is a [PEG
grammar](https://docs.rs/peg/latest/peg/).  PEGs are pretty cool. You can debug
the parser using [pegviz](https://github.com/fasterthanlime/pegviz); after
installing the pegviz tool, run `cargo run --release --features trace --
tests/l2s.out | pegviz --output pegviz.html` and then look at the generated
`pegviz.html` file. On the Rust side this just traces the grammar, and then
outside the trace is visualized into a parse trace.
