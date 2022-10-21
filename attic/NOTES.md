##### attempt0

just playing around with possible representations.

##### abstract-manual

Handwritten context, representation, visitors, and importer for
OCaml Types (CMI files); partial support for Typedtree (CMT files).

The actual implementation of types is hidden most of the type in the hope that
it will make user code more robust wrt changes.

##### abstract-astgen

DSL and generator to produce a representation and visitors very close to
abstract-manual.

##### next

Try a handwritten representation but keep it transparent: hopefully it should
be stable enough that it won't be a big problem for potential applications.

In return, we get types that are easier more intuitive (just "algebraic types"),
and we can derive the visitors using PPX.

The context should be reusable from previous experiments. Representation of
binding constructions still need some care!
