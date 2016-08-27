module aexpressionTests

open NUnit.Framework
open FsUnit
open aexpressions

let env = [("a", 3); ("c", 78); ("baf", 666); ("b", 111); ("x", 11); ("y", 9); ("v", 12); ("w", 55); ("z", 2)]
let emptyenv = []

[<Test>]
let ``Evaluate a multi and add`` () =
    let ev = Mul(Var "x", Add(Var "y", CstI 3))
    eval ev env |> should equal 132

[<Test>]
let ``Evaluate a sub with add`` () =
    let ev = Sub(Var "v", Add(Var "w", Var "z"))
    eval ev env |> should equal -45

[<Test>]
let ``Evaluate multi with sub and add`` () =
    let ev = Mul(CstI 2, Sub(Var "v", Add(Var "w", Var "z")))
    eval ev env |> should equal -90

[<Test>]
let ``Evaluate 4 additions`` () =
    let ev = Add(Var "x", Add(Var "y", Add(Var "z", Var "v")))
    eval ev env |> should equal 34

[<Test>]
let ``Format an addition`` () =
    let ev = Add(Var "x", CstI 34)
    fmt ev |> should equal "(x + 34)"

[<Test>]
let ``Format a subtraction`` () =
    let ev = Sub(Var "x", CstI 34)
    fmt ev |> should equal "(x - 34)"

[<Test>]
let ``Format a multiplication`` () =
    let ev = Mul(Var "x", CstI 34)
    fmt ev |> should equal "(x * 34)"

[<Test>]
let ``Simplify an addition with zero`` () =
    let ev = Add(CstI 0, Var "x")
    simplify ev |> should equal (Var "x")

[<Test>]
let ``Simplify reverse addition with zero`` () =
    let ev = Add(Var "x", CstI 0)
    simplify ev |> should equal (Var "x")

[<Test>]
let ``Simplify subtraction with zero`` () =
    let ev = Sub(Var "x", CstI 0)
    simplify ev |> should equal (Var "x")

[<Test>]
let ``Simplify multiplication with 1`` () =
    let ev = Mul(CstI 1, Var "x")
    simplify ev |> should equal (Var "x")

[<Test>]
let ``Simplify reverse multiplication with 1`` () =
    let ev = Mul(Var "x", CstI 1)
    simplify ev |> should equal (Var "x")

[<Test>]
let ``Simplify multiplication with 0`` () =
    let ev = Mul(CstI 0, Var "x")
    simplify ev |> should equal (CstI 0)

[<Test>]
let ``Simplify reverse multiplication with 0`` () =
    let ev = Mul(Var "x", CstI 0)
    simplify ev |> should equal (CstI 0)

[<Test>]
let ``Simplify equal subtraction`` () =
    let ev = Sub(Var "x", Var "x")
    simplify ev |> should equal (CstI 0)

[<Test>]
let ``Simplify with no effect`` () =
    let ev = Sub(Var "x", Var "y")
    simplify ev |> should equal (Sub(Var "x", Var "y"))