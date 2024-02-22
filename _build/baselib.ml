open Ast
open Mips

module Env = Map.Make(String)


let _types_ =  
  Env.of_seq
    (List.to_seq
       [ "%add", Func_t (Int_t, [ Int_t ; Int_t ])
       ; "%sub", Func_t (Int_t, [ Int_t ; Int_t ])
       ; "%mul", Func_t (Int_t, [ Int_t ; Int_t ])
       ; "%div", Func_t (Int_t, [ Int_t ; Int_t ])
       ; "%mod", Func_t (Int_t, [ Int_t ; Int_t ])
       ; "%equal", Func_t(Int_t, [ Int_t ; Int_t ])
       ; "%and", Func_t (Int_t, [ Int_t ; Int_t ])
       ; "%or", Func_t(Int_t, [ Int_t ; Int_t ])
       ; "%notequal", Func_t(Int_t, [ Int_t ; Int_t ])
       ; "%inf", Func_t (Bool_t, [ Int_t ; Int_t ])
       ; "%sup", Func_t (Bool_t, [ Int_t ; Int_t ])
       ; "%infeq", Func_t (Bool_t, [ Int_t ; Int_t ])
       ; "%supeq", Func_t (Bool_t, [ Int_t ; Int_t ])

       ; "puti",   Func_t(Int_t, [Int_t])
       ; "geti",   Func_t(Int_t, [Int_t])

    ])

let builtins =
  [ Label "_add"
  ; Lw (T0, Mem (SP, 0))
  ; Lw (T1, Mem (SP, 4))
  ; Add (V0, T0, T1)
  ; Jr RA
  ; Label "_mul"
  ; Lw (T0, Mem (SP, 0))
  ; Lw (T1, Mem (SP, 4))
  ; Mul (V0, T0, T1)
  ; Jr RA
  ; Label "_sub"
  ; Lw (T0, Mem (SP, 0))
  ; Lw (T1, Mem (SP, 4))
  ; Sub (V0, T0, T1)
  ; Jr RA
  ; Label "_div"
  ; Lw (T0, Mem (SP, 0))
  ; Lw (T1, Mem (SP, 4))
  ; Div (V0, T0, T1)
  ; Jr RA
  ; Label "_mod"
  ; Lw (T0, Mem (SP, 0))
  ; Lw (T1, Mem (SP, 4))
  ; Mod (V0, T0, T1)
  ; Jr RA
  ; Label "_equal"
  ; Lw (T0, Mem (SP, 0))
  ; Lw (T1, Mem (SP, 4))
  ; Equal (V0, T0, T1)
  ; Jr RA
  ; Label "_and"
  ; Lw (T0, Mem (SP, 0))
  ; Lw (T1, Mem (SP, 4))
  ; And (V0, T0, T1)
  ; Jr RA
  ; Label "_or"
  ; Lw (T0, Mem (SP, 0))
  ; Lw (T1, Mem (SP, 4))
  ; Or (V0, T0, T1)
  ; Jr RA
  ; Label "_notequal"
  ; Lw (T0, Mem (SP, 0))
  ; Lw (T1, Mem (SP, 4))
  ; Nequal (V0, T0, T1)
  ; Jr RA
  ; Label "_inf"
  ; Lw (T0, Mem (SP, 0))
  ; Lw (T1, Mem (SP, 4))
  ; Inf (V0, T0, T1)
  ; Jr RA
  ; Label "_sup"
  ; Lw (T0, Mem (SP, 0))
  ; Lw (T1, Mem (SP, 4))
  ; Inf (V0, T0, T1)
  ; Jr RA
  ; Label "_infeq"
  ; Lw (T0, Mem (SP, 0))
  ; Lw (T1, Mem (SP, 4))
  ; Infeq (V0, T0, T1)
  ; Jr RA
  ; Label "_supeq"
  ; Lw (T0, Mem (SP, 0))
  ; Lw (T1, Mem (SP, 4))
  ; Supeq (V0, T0, T1)
  ; Jr RA
  ; Label "puti"
  ; Lw (A0, Mem (SP, 0))
  ; Li (V0, Syscall.print_int)
  ; Syscall
  ; Jr RA
  ; Label "geti"
  ; Lw (A0, Mem (SP, 0))
  ; Li (V0, Syscall.read_int)
  ; Syscall
  ; Jr RA
  ; Label "puts"
  ; Lw (A0, Mem (SP, 0))
  ; Li (V0, Syscall.print_str)
  ; Syscall
  ; Jr RA
  ]
;;
