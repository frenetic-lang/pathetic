Pathetic: Path Frenetic
================================

Pathetic is a language for SDN forwarding policies specified at the path-level. For example, a policy can require that all SSH traffic traverses a specific IDS box:

```TcpDstPort = 22 => [*.IDS.*]```

For more details, see the FatTire paper, which builds upon Pathetic: <http://www.cs.cornell.edu/~reitblatt/papers/fault-tolerance-hotsdn13.pdf>

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
