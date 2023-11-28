# HW10

## Deadline: 12.12.2023, 23:59

### Assigned by @danyaberezun

## Manual Installation

1. [Install and initialize `opam`](https://opam.ocaml.org/doc/Install.html)
2. Create a `switch`: `opam switch create coq 4.14.0; opam switch set coq; eval $(opam env)`
3. Checkout the branch
4. Install dependencies: `./configure`
5. Run `make`. It should return an error: `Tactic failure: tauto failed`
6. Install and use one of IDEs/interfaces/plugins for Coq: [`coq-ide`](https://coq.inria.fr/refman/practical-tools/coqide.html), [`proof general`](https://proofgeneral.github.io/) or one of vs-code extensions ([coq](https://marketplace.visualstudio.com/items?itemName=ruoz.coq), [VsCoq](https://marketplace.visualstudio.com/items?itemName=maximedenes.vscoq), [Coq LSP](https://marketplace.visualstudio.com/items?itemName=ejgallego.coq-lsp))

## Task

* [`b1.v`](src/b1.v) is the home assignment
* [`b2.v`](src/b2.v) and [`b3.v`](src/b3.v) contain optional additional assignments for which additional points will be awarded (4 per file)

[Examples from the lecture](src/ex.v)
