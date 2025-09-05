# Monkey - Rust

An interpreter for the Monkey language as described by Thorsten Ball in his
excellent "Writing an interpreter in Go" book.

# Motivation

I did this project to learn Rust, so do not take this codebase an example of
good or idiomatic Rust code.

# Running the REPL

```
cargo build
./target/debug/monkey
```

To exit the REPL press CTRL-D

# Running a stored program

```
cargo build
./target/debug/monkey <program.monkey>
```

# Running tests

```
cargo test
```