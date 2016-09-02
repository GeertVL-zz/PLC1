module expressions

type expr =
    | CstI of int
    | Var of string
    | Prim of string * expr * expr
    | Let of string * expr * expr
    | If of expr * expr * expr

let rec lookup env x =
    match env with
    | []           -> failwith (x + " not found")
    | (y, v)::r    -> if x=y then v else lookup r x 

let rec lookOrSelf env x =
    match env with
    | [] -> Var x
    | (y, e)::r -> if x=y then e else lookOrSelf r x

let rec closedin (e : expr) (vs : string list) : bool =
    match e with
    | CstI i -> true
    | Var x  -> List.exists (fun y -> x = y) vs
    | Let(x, erhs, ebody) -> 
        let vs1 = x :: vs
        closedin erhs vs && closedin ebody vs1
    | Prim(ope, e1, e2) -> closedin e1 vs && closedin e2 vs
    
let mem x vs = List.exists (fun y -> x=y) vs

let rec union (xs, ys) =
    match xs with
    | [] -> ys
    | x::xr -> if mem x ys then union(xr, ys)
               else x :: union(xr, ys)

let rec minus (xs , ys) =
    match xs with
    | [] -> []
    | x :: xr -> if mem x ys then minus(xr, ys)
                 else x :: minus (xr, ys)

let rec freevars e : string list =
    match e with 
    | CstI i -> []
    | Var x -> [x]
    | Let(x, erhs, ebody) ->
        union (freevars erhs, minus(freevars ebody, [x]))
    | Prim(ope, e1, e2) -> union (freevars e1, freevars e2)

let closed2 e = (freevars e = [])

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
        | "=="  -> if i1 = i2 then 1 else 0
        | "+"   -> eval e1 env + eval e2 env
        | "-"   -> eval e1 env - eval e2 env 
        | "*"   -> eval e1 env * eval e2 env
        | _     -> failwith "operator does not exist"
    | If(e1, e2, e3) -> if (eval e1 env) <> 0 then eval e2 env else eval e3 env 
    | Let(x, erhs, ebody) ->
        let xval = eval erhs env
        let env1 = (x, xval) :: env
        eval ebody env1

let rec remove env x =
    match env with
    | [] -> []
    | (y, e)::r -> if x=y then r else (y, e) :: remove r x

let rec nsubst (e : expr) (env : (string * expr) list) : expr =
    match e with
    | CstI i -> e
    | Var x -> lookOrSelf env x
    | Let (x, erhs, ebody) -> 
        let newenv = remove env x
        Let (x, nsubst erhs env, nsubst ebody newenv)
    | Prim(ope, e1, e2) ->
        Prim(ope, nsubst e1 env, nsubst e2 env)

let newVar : string -> string =
    let n = ref 0
    let varMaker x = (n := 1 + !n; x + string (!n))
    varMaker

let rec subst (e : expr) (env : (string * expr) list) : expr =
    match e with
    | CstI i -> e
    | Var x -> lookOrSelf env x
    | Let(x, erhs, ebody) ->
        let newx = newVar x
        let newenv = (x, Var newx) :: remove env x
        Let(newx, subst erhs env, subst ebody newenv)
    | Prim(ope, e1, e2) -> Prim(ope, subst e1 env, subst e2 env)

type texpr =
    | TCstI of int
    | TVar of int
    | TLet of texpr * texpr
    | TPrim of string * texpr * texpr

let rec getindex env x = 
    match env with 
    | [] -> raise (Failure "Variable not found")
    | y::yr -> if x=y then 0 else 1 + getindex yr x;

let rec tcomp (e : expr) (cenv : string list) : texpr =
    match e with
    | CstI i -> TCstI i
    | Var x -> TVar (getindex cenv x)
    | Let(x, erhs, ebody) -> 
        let cenv1 = x :: cenv
        TLet(tcomp erhs cenv, tcomp ebody cenv1)
    | Prim(ope, e1, e2) ->
        TPrim(ope, tcomp e1 cenv, tcomp e2 cenv)

let rec teval (e : texpr) (renv : int list) : int =
    match e with
    | TCstI i -> i
    | TVar n -> List.nth renv n
    | TLet(erhs, ebody) -> 
        let xval = teval erhs renv
        let renv1 = xval :: renv
        teval ebody renv1
    | TPrim("+", e1, e2) -> teval e1 renv + teval e2 renv
    | TPrim("*", e1, e2) -> teval e1 renv * teval e2 renv
    | TPrim("-", e1, e2) -> teval e1 renv - teval e2 renv
    | TPrim _            -> failwith "unknown primitive"


