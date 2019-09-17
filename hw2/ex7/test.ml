open Zexpr
open Unix

let (|>) g f = f g

let test : expr -> unit = fun exp ->
  let _ = print_value (eval (emptyEnv, exp)) in
    ()

let _ = print_Value (eval (empty Env, MULT (NUM 3, DIVIDE (NUM 6, NUM 2))))
