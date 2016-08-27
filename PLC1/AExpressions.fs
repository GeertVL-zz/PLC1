module aexpressions

type aexpr =
    | CstI of int
    | Var of string
    | Add of aexpr * aexpr
    | Mul of aexpr * aexpr
    | Sub of aexpr * aexpr

let rec lookup env x =
    match env with
    | []           -> failwith (x + " not found")
    | (y, v)::r    -> if x=y then v else lookup r x 

let rec eval (e : aexpr) (env: (string * int) list) : int =
    match e with
    | CstI i -> i
    | Var x  -> lookup env x
    | Add(e1, e2) -> eval e1 env + eval e2 env
    | Mul(e1, e2) -> eval e1 env * eval e2 env
    | Sub(e1, e2) -> eval e1 env - eval e2 env

let rec fmt (e : aexpr) : string =
    match e with
    | CstI i -> string i
    | Var x -> x
    | Add(e1, e2) -> sprintf "(%s + %s)" (fmt e1) (fmt e2)
    | Sub(e1, e2) -> sprintf "(%s - %s)" (fmt e1) (fmt e2)
    | Mul(e1, e2) -> sprintf "(%s * %s)" (fmt e1) (fmt e2)

let simplify (e : aexpr) : aexpr =
    match e with
    | Add(CstI 0, e1) -> e1
    | Add(e1, CstI 0) -> e1
    | Sub(e1, CstI 0) -> e1
    | Mul(CstI 1, e1) -> e1
    | Mul(e1, CstI 1) -> e1
    | Mul(CstI 0, e1) -> CstI 0
    | Mul(e1, CstI 0) -> CstI 0
    | Sub(e1, e2) when e1 = e2 -> CstI 0
    | _ -> e