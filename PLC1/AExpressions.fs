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