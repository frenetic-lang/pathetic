FatTire: Fault Tolerating Regular Expressions
================================

FatTire is a language for specifying fault-tolerant network forwarding policies. For more details, see the paper: <http://www.cs.cornell.edu/~reitblatt/papers/fault-tolerance-hotsdn13.pdf>

Building from Source
====================

Prerequisites
-------------

- OCaml 4 or higher <http://caml.inria.fr/download.en.html>

- OPAM <http://opam.ocamlpro.com>

- The following OCaml libraries:

  - findlib
  - lwt
  - cstruct 
  - oUnit
  - menhir
  - frenetic

  These are available on OPAM:

  ```
  $ opam install ocamlfind cstruct lwt ounit menhir frenetic
  ```

Building
--------

- From the ocaml/ directory of the repository, run `make`

  ```
  $ make
  ```

Hacking
=======

OCaml Wisdom
------------

If you're using the user-mode reference switch, emit CONTROLLER actions last.
