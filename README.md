
# Issues

### min_int numeral

Monga's integer numerals are being stored as OCaml's integer after lexing. OCaml's `max_int` and `min_int` are, respectively, `4611686018427387903` and `-4611686018427387904`. As the lexer cannot decide whether '-' is a unary or binary expression, it sees negative numerals as two separate tokens. The issue is that `min_int`, without the sign, doesn't fit in OCaml's integer. In other words, `-4611686018427387904` is lexed first as the token '-' and then as a numeral, which fails because it exceeds `max_int`.


# Installing

This project has Ocaml, [Dune](https://github.com/ocaml/dune) and [Menhir](http://gallium.inria.fr/~fpottier/menhir/) as dependencies. The easiest way to install those is through opam.


### Installing opam

The first step is to [install opam](https://opam.ocaml.org/doc/Install.html). The easiest way is to download the binaries from [here](https://github.com/ocaml/opam/releases) put it in your PATH as opam, and set it as executable, e.g.
```
$ sudo cp <downloaded file> /usr/local/bin/opam
$ sudo chmod a+x /usr/local/bin/opam
```
For this project, running `opam init` is not required.

Verify install:
```
$ opam --version
2.0.5
```

### Dependencies.

At the project's root, run this command to install the OCaml's base compiler and all other dependencies. It may take a while.
```
$ opam switch create . 
```

After that, at the project's root, run `$ eval $(opam env)` to make sure our environment is properly synced. You'll need to run this every session.

Running `$ opam switch` should display the project's as the as the selected switch.

Verify install:
```
$ dune --version
1.11.2
```

# Running tests

Run `dune clean && dune runtest` at the project's root. It will run all tests and diff their output with the expected values.

