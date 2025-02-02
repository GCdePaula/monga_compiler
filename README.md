
# Issues

### Functions with unreachable statements must still end with return
Functions with the follwing body:

```
if cond {
  return 1;
}
else {
  return 0;
}

return -1; # unreachable, but required.
```
Must explicitly end with a return statement (the compiler won't gracefully tell you), after the if statement. The generated LLVM code (with the useless return) has basic blocs without predecessors.

### Functions without a return type must explicitly have an empty return statement.
If they do, it will fail ungracefully at the LLVM level.

### Functions with statements after a return statement are typed correctly, but generate bad LLVM code.
Functions cannot have a statements after a return statement. If they do, it will fail ungracefully at the LLVM level.

### Functions without return are typed correctly
Functions that have a return type can currently return nothing. Type checking correctly disallows wrong returns, but does not check if there actually is a return statement. If they do, it will fail ungracefully at the LLVM level.

### Integer numeral minimum value
Monga's integer numerals are being stored as OCaml's integer after lexing. OCaml's `max_int` and `min_int` are `4611686018427387903` and `-4611686018427387904`, respectively. As the lexer cannot decide whether the token `-` is a unary or binary expression, it sees negative numerals as two separate tokens. The issue is that `min_int`, without the sign, doesn't fit in OCaml's integer. In other words, `-4611686018427387904` is lexed first as the token `-` and then as a numeral, which fails because it exceeds `max_int`. Therefore the smallest integer in Monga cannot be represented by an integer numeral.


# Installing

This project has the following dependencies:

1. Ocaml
2. [Dune](https://github.com/ocaml/dune)
3. [Menhir](http://gallium.inria.fr/~fpottier/menhir/)
4. Jane Street's [base](https://github.com/janestreet/base) and [stdio](https://github.com/janestreet/stdio)
5. The Llvm bindings for OCaml

The easiest way to install those is through opam. Also, you must already have Llvm installed on your computer.


### Installing opam

The first step is to [install opam](https://opam.ocaml.org/doc/Install.html). The easiest way is to download the binaries from [here](https://github.com/ocaml/opam/releases), put it in your PATH as opam, and set it as executable, e.g.
```
$ sudo cp <downloaded file> /usr/local/bin/opam
$ sudo chmod a+x /usr/local/bin/opam
```
For this project, running `$ opam init` is not required.

Verify install:
```
$ opam --version
2.0.5
```

### Dependencies

First install `m4` with `$ sudo apt-get install m4`, or similar.

At the project's root, run the following to install OCaml's base compiler and all other dependencies for this project only. It will take a while.
```
$ opam switch create .
```

After that, at the project's root, run `$ eval $(opam env)` to make sure our environment is properly synced. You'll need to run this every session.

Running `$ opam switch` should display the project's as the as the selected switch.

Verify install:
```
$ ocaml --version && dune --version && menhir --version
The OCaml toplevel, version 4.08.1
1.11.2
menhir, version 20190626
```

# Running tests

Run `$ dune clean && dune runtest` at the project's root. It will run all tests and diff their output with the expected values.

