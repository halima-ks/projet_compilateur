%{
  open Ast
  open Ast.Syntax
%}

%token <int> Lint 
%token Ltint Ltbool Ltstring
%token <string> Lvar Lstring
%token <bool> Lbool
%token Lsc Lend 
%token Ladd Lsub Ldiv Lmul Lmod
%token Lreturn Lassign 
%token Lpo Lpf Lao Laf 
%token Lwhile Lif Lelse
%token Leq LnotEq Lsup Linf LsupEq LinfEq 
%token Land Lor


 
%left Ladd Lsub
%left Lmul Ldiv Lmod
%left Leq LnotEq
%left LinfEq Linf LsupEq Lsup 

%start block

%type <Ast.Syntax.block> block

%%

block:
| e = instr; b = block{
  [e] @ b
}
| e = instr { [e] }
| Lend {[]}
;

expr:
| n = Lint {
  Int { value = n 
      ; pos = $startpos(n) }
}
| n = Lbool {
  Bool { value = n 
      ; pos = $startpos(n) }
}
| s = Lstring {
    String { value = s; pos = $startpos(s) }
  }


| v = Lvar{
  Var {name = v; pos = $startpos(v)}
}
| a = expr; Lmul; b = expr {
  Call {  func = "%mul"
          ;args = [a ; b]
          ;pos  = $startpos($2) }}

| a = expr; Ladd; b = expr {
  Call {  func = "%add"
          ;args = [a ; b]
          ;pos  = $startpos($2) }}

| a = expr; Lsub; b = expr {
  Call {  func = "%sub"
          ;args = [a ; b]
          ;pos  = $startpos($2) }}

| a = expr; Ldiv; b = expr {
  Call {  func = "%div"
          ;args = [a ; b]
          ;pos  = $startpos($2) }} 
           
| a = expr; Lmod; b = expr {
  Call {  func = "%mod"
          ;args = [a ; b]
          ;pos  = $startpos($2) }}  

| a = expr; Leq; b = expr {
  Call {  func = "%equal"
          ;args = [a ; b]
          ;pos  = $startpos($2) }} 

| a = expr; LnotEq; b = expr {
  Call {  func = "%notequal"
          ;args = [a ; b]
          ;pos  = $startpos($2) }}  

| a = expr; Lsup; b = expr {
  Call {  func = "%sup"
          ;args = [a ; b]
          ;pos  = $startpos($2) }} 

| a = expr; Linf; b = expr {
  Call {  func = "%inf"
          ;args = [a ; b]
          ;pos  = $startpos($2) }} 

| a = expr; LinfEq; b = expr {
  Call {  func = "%infeq"
          ;args = [a ; b]
          ;pos  = $startpos($2) }}

| a = expr; LsupEq; b = expr {
  Call {  func = "%supeq"
          ;args = [a ; b]
          ;pos  = $startpos($2) }} 

| a = expr; Land; b = expr {
  Call {  func = "%and"
          ;args = [a ; b]
          ;pos  = $startpos($2) }}

| a = expr; Lor; b = expr {
  Call {  func = "%or"
          ;args = [a ; b]
          ;pos  = $startpos($2) }}   

; 
  

instr:

| v = Lvar; Lassign; b = expr; Lsc{
  Assign {name = v
        ; expr = b 
        ; pos = $startpos(v)}}

|Ltint; v = Lvar; Lsc{
  Decl {tp = Int_t 
      ; name = v
      ; pos = $startpos(v) } }

|Ltbool; v = Lvar; Lsc{
  Decl {tp = Bool_t 
      ; name = v
      ; pos = $startpos(v) } }

| Ltstring; v = Lvar; Lsc{
    Decl {tp = Str_t 
        ; name = v
        ; pos = $startpos(v) } }



| Lreturn; e = expr; Lsc{
  Return {exp = e
         ;pos = $startpos($1) }}

| Lif; Lpo; e = expr; Lpf;Lao; b = block; Laf ; Lelse; Lao; be = block; Laf{
  If {exp = e
    ; if_ = b
    ; else_ = be
    ; pos = $startpos($1)}}

| Lwhile; Lpo; e = expr; Lpf; Lao; bl = block; Laf{
  While { exp = e
        ; b = bl
        ; pos = $startpos(e)}}
;




