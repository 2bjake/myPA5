
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
  x: Int;
  y: Bool;
  z: String;
};

class Bar inherits Foo {
  a: Int;
  b: Foo;
};


class Main inherits IO {
  main():Int { 0 };
};

