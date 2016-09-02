module Stack

type rinstr =
    | RCstI of int
    | RAdd
    | RSub
    | RMul
    | RDup
    | RSwap

let rec reval (inss : rinstr list) (stack : int list) : int =
    match (inss, stack) with
    | ([], v :: _) -> v
    | ([], [])     -> failwith "reval: no result on stack!"
    | (RCstI i :: insr, stk) -> reval insr (i::stk)
    | (RAdd :: insr, i2 :: i1 :: stkr) -> reval insr ((i1 + i2)::stkr)
    | (RSub :: insr, i2 :: i1 :: stkr) -> reval insr ((i1 - i2)::stkr)
    | (RMul :: insr, i2 :: i1 :: stkr) -> reval insr ((i1 * i2)::stkr)
    | (RDup :: insr, i1 :: stkr) -> reval insr (i1 :: i1 :: stkr)
    | (RSwap :: insr, i2 :: i1 :: stkr) -> reval insr (i1 :: i2 :: stkr)
    | _ -> failwith "reval: too few operands on stack"

