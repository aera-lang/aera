# Aera Programming Language

> *Where ideas take shape.*

Aera is an expression-oriented programming language centered around clarity and simplicity. There should be little to no friction when designing and writing code. 

It should just feel right.

Aera's design is shaped by the needs of interactive applications such as games and GUIs. The goal is not just to write code, but to shape ideas into systems that stay clear and understandable as they grow.

Aera aims to be statically typed with automatic memory management, leaning into functional programming concepts like immutable state and algebraic data types. These are design goals the language is actively working towards. See [Current Status](#current-status) for what's been implemented today.

Aera takes inspiration from modern C++, Rust, OCaml, Swift and Go.

## Quick Example

Aera currently ships as a REPL. This is a small snippet of what currently works today:

```
~> fn add(a: int32, b: int32) -> int32 { a + b }
<function add(a: int32, b: int32)>
~> let x = add(1, 2)
unit
~> x
3
~> let y = if x > 2 { "big" } else { "small" }
unit
~> y
"big"
```

## Current Status

Aera is ***VERY*** early! 

This is a first, barebones snapshot (`v0.0.1`), not a stable release. The REPL supports function declarations, `let`/`const` bindings, arithmetic and logical expressions, `if`/`while`/`loop`, and function calls.

## Limitations

This is a first snapshot, so *please* treat it accordingly. 

The parser has no error recovery yet, so one bad token stops it cold instead of pointing you at the problem and moving on. Type annotations are parsed but nothing checks them. There's no way to group data yet, so no tuples and no arrays. And there's no `print` or any I/O to speak of, since the module system needs to be implemented first.

## Installation

Clone the repo and run it with `make`:

```sh
git clone https://github.com/you/aera.git
cd aera
make run
```

This builds and launches the REPL. There's also `make interpret FILE=path/to/file.aera`, but the interpreter path is far less stable. You can use it, but expect things not to work perfectly.


Prebuilt binaries are available on the [Releases](https://github.com/aera-lang/aera/releases) page. For now, only a Windows binary is provided, so macOS/Linux users should build from source (see above).

Download `aera.exe`, then run it directly:

```sh
./aera.exe
```

Or just double-click it in File Explorer.

## Documentation

For complete language documentation, see the [`docs/spec/README.md`](docs/spec/) directory.

> Note: Documentation will soon move to Aera's website ([`aera.codes`](https://aera.codes)).

## License

Aera is under the Apache 2.0 License.