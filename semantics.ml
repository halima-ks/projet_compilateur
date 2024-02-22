open Ast
open Ast.V1
open Ast.IR1
open Baselib

exception Error of string * Lexing.position

let expr_pos expr =
  match expr with
  | Syntax.Int n   -> n.pos
  | Syntax.Var v   -> v.pos
  | Syntax.Call c  -> c.pos 

let errt expected given pos =
  raise (Error (Printf.sprintf "expected %s but given %s"
                  (string_of_type_t expected)
                  (string_of_type_t given),
                pos))
                
let rec analyze_expr expr env =
  match expr with
  | Syntax.Int n  -> Value (Int n.value), Int_t
  | Syntax.Bool n -> Value (Bool n.value), Bool_t
  | Syntax.Var v  ->
    if Env.mem v.name env then
      Var v.name, Env.find v.name env
    else
      raise (Error (Printf.sprintf "unbound variable '%s'" v.name,
                    v.pos))
 | Syntax.Call c ->
    match Env.find_opt c.func env with
    | Some (Func_t (rt, at)) ->
       if List.length at != List.length c.args then
         raise (Error (Printf.sprintf "expected %d arguments but given %d"
                         (List.length at) (List.length c.args), c.pos)) ;
       let args = List.map2 (fun eat a -> let aa, at = analyze_expr a env
                                          in if at = eat then aa
                                             else errt eat at (expr_pos a))
                    at c.args in
       Call (c.func, args), rt
    | Some _ -> raise (Error (Printf.sprintf "'%s' is not a function" c.func,
                              c.pos))
    | None -> raise (Error (Printf.sprintf "undefined function '%s'" c.func,
                            c.pos))
                            
let rec analyze_instr instr env =
  match instr with
  | Syntax.Return r ->
    let ae, _ = analyze_expr r.exp env in
    Return ae, env
    
  | Syntax.Assign a ->
    let ae, et = analyze_expr a.expr env in
    Assign (LVar(a.name), ae), Env.add a.name et env

  | Syntax.Decl d -> Decl (d.tp, d.name), (Env.add d.name d.tp env)

  | Syntax.If i -> let (a, b) = analyze_expr i.exp env in 
    If (a, analyze_block i.if_ env,  analyze_block i.else_ env), env

  | Syntax.While w -> let (a, b) = analyze_expr w.exp env in
    While (a, analyze_block w.b env), env


and analyze_block block env =
  match block with
  | [] -> []
  | instr :: rest ->
      let ai, new_env = analyze_instr instr env in
      ai :: (analyze_block rest new_env)
      

let analyze parsed =
  analyze_block parsed _types_
