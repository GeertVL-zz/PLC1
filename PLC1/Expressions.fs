module expressions

type expr =
    | CstI of int
    | Var of string
    | Prim of string * expr * expr

let rec lookup env x =
    match env with
    | []           -> failwith (x + " not found")
    | (y, v)::r    -> if x=y then v else lookup r x 

let rec eval (e : expr) (env: (string * int) list) : int =
    match e with
    | CstI i -> i
    | Var x  -> lookup env x
    | Prim(ope, e1, e2) -> 
        let i1 = eval e1 env
        let i2 = eval e2 env
        match ope with
        | "max" -> if i1 > i2 then i1 else i2
        | "min" -> if i1 < i2 then i1 else i2
        | "+"   -> eval e1 env + eval e2 env
        | "-"   -> eval e1 env - eval e2 env 
        | "*"   -> eval e1 env * eval e2 env
        | _     -> failwith "operator does not exist"



