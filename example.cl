
(*  Example cool program testing as many aspects of the code generator
    as possible.
 *)

class Foo inherits Main {
  myFunc(): Int { 0 };
  my2Func(): Int { 0 };
};

class Bar inherits Foo {
    myFunc(): Int { 0 };
};

class Main inherits IO {
  main():Int { 0 };
};

