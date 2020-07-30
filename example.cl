
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
  xb: Int <- x;
  x: Int <- 2;
  x2: Int <- x;
  y: Bool <- true;
  z: String <- "hi";
  f(x: Int): Int { x };
  g(x: Int): Int { 0 };
};

class Bar inherits Foo {
  a: Int;
  b: Foo;
  z2: Bool <- true;
  f(x: Int): Int { x };
};



class Main inherits IO {
  x: Foo <- new Foo;
  y: Int <- 2;
  main():Int { let z: Int <- 2 in y + z };
};

