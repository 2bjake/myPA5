
(*  Example cool program testing as many aspects of the code generator
    as possible.
 *)
(*
class Foo inherits Main {
  myFunc(): Int { 0 };
  my2Func(): Int { 0 };
};

class Bar inherits Foo {
    myFunc(): Int { 0 };
};
*)

class Foo {
  x: Int <- 2;
  y: Bool <- true;
  z: String <- "hi";
};

class Bar inherits Foo {
  a: Int;
  b: Foo;
  f(x: Int): Int { 0 };
};


class Main inherits IO {
  main():Int { 0 };
};

