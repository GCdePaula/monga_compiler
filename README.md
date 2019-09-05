

# Installing

### opam

The first step is to [install opam](https://opam.ocaml.org/doc/Install.html). Good luck.

Verify install:
```
$ opam --version
2.0.5
```

### OCaml

At the project's root, run this command to install the OCaml's base compiler on the current directory. It may take a while.
```
$ opam switch create . ocaml-base-compiler.4.08.1
```

After that, run `$ eval $(opam env)` to make sure our environment is properly synced. You'll need to run this every session, or add ``eval `opam config env` `` to the configuration file of your shell (for example, `~/.bashrc` or `~/.zshrc`)

Running `$ opam switch` should display the project's as the as the selected switch.


### Dune

Install Dune using opam.
```
$ opam install dune
```

Verify install:
```
$ dune --version
1.11.2
```

# Running tests

Simply run `dune clean && dune runtest` at the project's root. It will run all tests and see if the outputs match the expected values.




