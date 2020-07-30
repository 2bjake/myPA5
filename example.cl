
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

class Baz inherits Bar {};

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
  a: Int <- 1 + 2;
  b: Foo;
  z2: Bool <- true;
  f(x: Int): Int { x };
};

class Main inherits IO {
  x: Foo <- new Foo;
  y: Int <- 2;
  z: Baz <- new Baz;
  main():Int {
    case z of
      o: Object => 0;
      s: String => 3;
      f: Foo => 1;
      b: Bar => 2;
    esac
  };
};
