

type type_t =
| Int_t
| Bool_t 
| Str_t 
| Func_t of type_t * type_t list

let rec string_of_type_t t =
match t with
  | Int_t  -> "int"
  | Bool_t -> "bool"
  | Str_t  -> "char" 
  | Func_t (r, a) ->
    (if (List.length a) > 1 then "(" else "")
    ^ (String.concat ", " (List.map string_of_type_t a))
    ^ (if (List.length a) > 1 then ")" else "")
    ^ " -> " ^ (string_of_type_t r)

module Syntax = struct
  type ident = string
  type expr =
    | Int of { value: int
             ; pos: Lexing.position } 
    | Bool of { value: bool
             ; pos: Lexing.position } 
    | String of { value: string
             ; pos: Lexing.position } 
          
    | Var  of { name: ident
              ; pos: Lexing.position }  
    | Call of {func : ident
              ;args : expr list
              ;pos : Lexing.position} 
    type instr =
    | Return of { exp: expr
              ; pos: Lexing.position }
    | Assign of { name: ident
              ; expr: expr
              ; pos: Lexing.position }
    | Decl of { tp : type_t
              ;name : ident   
              ;pos : Lexing.position}
  
    | If of {  exp : expr
              ; if_  : block
              ; else_  : block
              ; pos : Lexing.position 
    }
    | While of { exp : expr
               ; b    : block
               ;pos   :Lexing.position 
     }
    | For of { init : expr
              ;cond : expr
              ;post : expr
              ;boby : block
    }
    and block = instr list
end
 
     
module type Parameters = sig
  type value
end

module V1 = struct
  type value =
    | Nil
    | Bool of bool
    | Int  of int
    | Str  of string
end

module V2 = struct
  type value =
    | Nil
    | Bool of bool
    | Int  of int
    | Data of string
end

module IR (P : Parameters) = struct
  type ident = string
  type expr =
    | Value of P.value
    | Var   of ident
    | Call  of ident * expr list
  type lvalue =
    | LVar  of ident
    | LAddr of expr
  type instr =
    | Return of expr
    | Assign of lvalue * expr
    | Decl   of type_t * ident
    | If   of expr * block * block
    | While of expr * block 
    | For of expr * expr * expr * block
  and block = instr list
  type def =
    | Func of ident * ident list * block
  type prog = def list
end

module IR1 = IR(V1)
module IR2 = IR(V2)
