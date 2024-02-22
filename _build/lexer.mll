{
  open Lexing
  open Parser

  exception Error of char
}
let alpha = ['a'-'z' 'A'-'Z']
let num = ['0'-'9']
let identifier = alpha (alpha | num | '-' | '_')*

rule token = parse
| eof             { Lend }
| [ ' ' '\t' ]    { token lexbuf }
| '\n'            { Lexing.new_line lexbuf; token lexbuf }
| '#'             { comment lexbuf }
| "return"        { Lreturn}
| ";"             { Lsc}
| num+  as n      { Lint (int_of_string n)}

(* types *)
| "true"   as t   { Lbool (bool_of_string t) }
| "false"  as f   { Lbool (bool_of_string f) }
| "int"           { Ltint}
| "bool"          { Ltbool}
| "string"        { Ltstring }

(* operateurs arithmitiques *)
| "*"             { Lmul}
| "+"             { Ladd }
| "-"             { Lsub}
| "/"             { Ldiv}
| "="             { Lassign}
| "%"             { Lmod}

(* operateurs de comparaison *)
| "!="            { LnotEq}
| "<"             { Linf}
| ">"             { Lsup}
| "<="            { LinfEq}
| ">="            { LsupEq}
| "=="            { Leq}

(* operateurs logiques *)
| "||"            { Lor}
| "&&"            { Land}

| "{"             { Lao }
| "}"             { Laf}
| "("             { Lpo }
| ")"             { Lpf}

(* boucle et condition *)
| "if"            { Lif}
| "else"          { Lelse}
| "while"         { Lwhile}
| identifier as v { Lvar (v)}
| _ as c          { raise (Error c) }

and comment = parse
| eof  { Lend }
| '\n' { Lexing.new_line lexbuf; token lexbuf }
| _    { comment lexbuf }

