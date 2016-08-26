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
let ``Evaluate a non existing operator`` () =
    let ev = Prim("A+", CstI 7, CstI 8)
    (fun() -> eval ev env |> ignore) |> should throw typeof<System.Exception>

[<Test>]
let ``Evaluate an addition`` () =
    let ev = Prim("+", CstI 5, Var "a")
    eval ev env |> should equal 8

[<Test>]
let ``Evaluate a subtraction`` () =
    let ev = Prim("-", CstI 5, Var "a")
    eval ev env |> should equal 2

[<Test>]
let ``Evaluate a multiplication`` () =
    let ev = Prim("*", CstI 5, Var "a")
    eval ev env |> should equal 15

[<Test>]
let ``Evaluate maximum of two numbers`` () =
    let ev = Prim("max", CstI 5, Var "a")
    eval ev env |> should equal 5

[<Test>]
let ``Evaluate minimum of two numbers`` () =
    let ev = Prim("min", CstI 5, Var "a")
    eval ev env |> should equal 3

[<Test>]
let ``Two different numbers are not equal`` () =
    let ev = Prim("==", CstI 5, Var "a")
    eval ev env |> should equal 0

[<Test>]
let ``Evaluate condition that is true`` () =
    let ev = If(Var "a", CstI 11, CstI 22)
    eval ev env |> should equal 11

[<Test>]
let ``Evaluate condition that is false`` () =
    let ev = If (CstI 0, CstI 11, CstI 22)
    eval ev env |> should equal 22


