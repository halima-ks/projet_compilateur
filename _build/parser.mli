
(* The type of tokens. *)

type token = 
  | Lwhile
  | Lvar of (string)
  | Ltstring
  | Ltint
  | Ltbool
  | LsupEq
  | Lsup
  | Lsub
  | Lstring of (string)
  | Lsc
  | Lreturn
  | Lpo
  | Lpf
  | Lor
  | LnotEq
  | Lmul
  | Lmod
  | Lint of (int)
  | LinfEq
  | Linf
  | Lif
  | Leq
  | Lend
  | Lelse
  | Ldiv
  | Lbool of (bool)
  | Lassign
  | Lao
  | Land
  | Laf
  | Ladd

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val block: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.Syntax.block)
