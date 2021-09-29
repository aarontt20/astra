Check that .bc.js rule is generated only if js mode is used.

  $ dune build --display short a.bc.js
  Error: Don't know how to build a.bc.js
  Hint: did you mean b.bc.js or e.bc.js?
  [1]

  $ dune build --display short b.bc.js
   js_of_ocaml .js/stdlib/std_exit.cmo.js
   js_of_ocaml b.bc.runtime.js
      ocamldep .b.eobjs/b.ml.d
        ocamlc .b.eobjs/byte/b.{cmi,cmo,cmt}
   js_of_ocaml .js/stdlib/stdlib.cma.js
   js_of_ocaml .b.eobjs/byte/b.cmo.js
   js_of_ocaml b.bc.js

We also check that .cmo.js rules are not generated if not specified.

  $ dune build --display short _build/default/.a.eobjs/byte/a.cmo.js
  Error: Don't know how to build _build/default/.a.eobjs/byte/a.cmo.js
  [1]

JS compilation of libraries is always available to avoid having to annotate
every dependency of an executable.

  $ dune build --display short _build/default/.foo.objs/foo.cma.js
        ocamlc .foo.objs/byte/foo.{cmi,cmo,cmt}
      ocamldep .foo.objs/c.ml.d
        ocamlc .foo.objs/byte/foo__C.{cmi,cmo,cmt}
        ocamlc foo.cma
   js_of_ocaml .foo.objs/foo.cma.js

Check that js targets are attached to @all, but not for tests that do not
specify js mode (#1940).

  $ dune clean
  $ dune build --display short @@all 2>&1 | grep js_of_ocaml
   js_of_ocaml .js/stdlib/std_exit.cmo.js
   js_of_ocaml b.bc.runtime.js
   js_of_ocaml e.bc.runtime.js
   js_of_ocaml .js/stdlib/stdlib.cma.js
   js_of_ocaml .b.eobjs/byte/b.cmo.js
   js_of_ocaml b.bc.js
   js_of_ocaml .foo.objs/foo.cma.js
   js_of_ocaml .e.eobjs/byte/e.cmo.js
   js_of_ocaml e.bc.js

Check that building a JS-enabled executable that depends on a library works.

  $ dune clean
  $ dune build --display short e.bc.js
   js_of_ocaml .js/stdlib/std_exit.cmo.js
   js_of_ocaml e.bc.runtime.js
      ocamldep .e.eobjs/e.ml.d
        ocamlc .foo.objs/byte/foo.{cmi,cmo,cmt}
      ocamldep .foo.objs/c.ml.d
   js_of_ocaml .js/stdlib/stdlib.cma.js
        ocamlc .foo.objs/byte/foo__C.{cmi,cmo,cmt}
        ocamlc .e.eobjs/byte/e.{cmi,cmo,cmt}
        ocamlc foo.cma
   js_of_ocaml .e.eobjs/byte/e.cmo.js
   js_of_ocaml .foo.objs/foo.cma.js
   js_of_ocaml e.bc.js
