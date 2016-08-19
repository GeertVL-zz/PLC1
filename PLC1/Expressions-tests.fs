module expressionTests

open NUnit.Framework
open FsUnit
open expressions

let env = [("a", 3); ("c", 78); ("baf", 666); ("b", 111); ("x", 11)]
let emptyenv = []

[<Test>]
let ``first test`` () =
    1 |> should equal 1

[<Test>]
let ``Lookup an environment variable`` () =
    lookup env "x" |> should equal 11

[<Test>]
let ``Evaluate a value`` () =
    let ev = CstI 5
    eval ev emptyenv |> should equal 5

[<Test>]
let ``Evaluate a variable`` () =
    let ev = Var "x"
    eval ev env |> should equal 11

[<Test>]
let ``Evaluate an addition`` () =
    let ev = Prim("+", CstI 5, Var "a")
    eval ev env |> should equal 8

[<Test>]
let ``Evaluate a subtraction`` () =
    let ev = Prim("-", CstI 5, Var "a")
    eval ev env |> should equal 2

[<Test>]
let ``Evaluate maximum of two numbers`` () =
    let ev = Prim("max", CstI 5, Var "a")
    eval ev env |> should equal 5



