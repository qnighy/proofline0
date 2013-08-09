# proofline0: study for making programming language, with linear logic

proofline0 is an imitation of Coq's type system (PTS).
One difference is: This has linear types.

This was made for study and not for using. There is only minimum functionality.

## Usage

```
sudo apt-get install ocaml-nox
make
./proofline0 < test.proofline0
./proofline0 < test2.proofline0
```

## Syntax

A program should be written in one file. There are no module systems.

A program consists of several instructions. instruction is one of following:

```
Definition id : type := def.
Axiom id : type.
```

A term is one of the following:
```
t1 t2
forall id : t1, t2
t1 -> t2
t1 -@ t2
fun id : t1 => t2
fun *id : t1 => t2
let id : t1 := t2 in t3
let *id : t1 := t2 in t3
id
(term)
Type(universe_level)
Prop
```

- "t1 -> t2" is a syntax sugar for "forall _ : t1, t2".
- "Prop" is a syntax sugar for "Type(0)".

You can't omit type annotation. There are no type inference system.


* Semantics

Almost same as Coq. only difference is linear type.

- type of ``fun *id : t1 => t2`` is ``A -@ B``.
- when ``id`` is declared with asterisk, you can use ``id`` just once.
- when ``id`` is declared with asterisk, you can't use ``id``
  within non-linear application "t1 t2", where type of t1 is "A -> B".

* Samples

- test.proofline0: church encoding of usual intuitionistic logic.
- test2.proofine0: church encoding of intuitionistic linear logic.


