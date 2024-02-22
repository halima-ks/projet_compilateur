type reg =
  | Zero
  | SP
  | RA
  | FP
  | V0
  | A0
  | A1
  | T0
  | T1

type label = string

type loc =
  | Lbl of label
  | Mem of reg * int

type instr =
  | Label of label
  | Li    of reg * int
  | La    of reg * loc
  | Sw    of reg * loc
  | Lw    of reg * loc
  | Sb    of reg * loc
  | Lb    of reg * loc
  | Move  of reg * reg
  | Addi  of reg * reg * int
  | Add   of reg * reg * reg
  | Mul   of reg * reg * reg
  | Sub   of reg * reg * reg
  | Div   of reg * reg * reg
  | Mod   of reg * reg * reg
  | Equal   of reg * reg * reg
  | And   of reg * reg * reg
  | Or   of reg * reg * reg
  | Nequal   of reg * reg * reg
  | Inf   of reg * reg * reg
  | Sup   of reg * reg * reg
  | Infeq   of reg * reg * reg
  | Supeq   of reg * reg * reg

  | Syscall
  | B     of label
  | Beqz  of reg * label
  | Jal   of label
  | Jr    of reg

type directive =
  | Asciiz of string

type decl = label * directive

type asm = { text: instr list ; data: decl list }

module Syscall = struct
  let print_int = 1
  let print_str = 4
  let read_int  = 5
  let read_str  = 8
  let sbrk      = 9
end

let ps = Printf.sprintf (* alias raccourci *)

let fmt_reg = function
  | Zero -> "$zero"
  | SP   -> "$sp"
  | FP   -> "$fp"
  | RA   -> "$ra"
  | V0   -> "$v0"
  | A0   -> "$a0"
  | A1   -> "$a1"
  | T0   -> "$t0"
  | T1   -> "$t1"

let fmt_loc = function
  | Lbl (l)    -> l
  | Mem (r, o) -> ps "%d(%s)" o (fmt_reg r)

let fmt_instr = function
  | Label (l)        -> ps "%s:" l
  | Li (r, i)        -> ps "  li %s, %d" (fmt_reg r) i
  | La (r, a)        -> ps "  la %s, %s" (fmt_reg r) (fmt_loc a)
  | Sw (r, a)        -> ps "  sw %s, %s" (fmt_reg r) (fmt_loc a)
  | Lw (r, a)        -> ps "  lw %s, %s" (fmt_reg r) (fmt_loc a)
  | Sb (r, a)        -> ps "  sb %s, %s" (fmt_reg r) (fmt_loc a)
  | Lb (r, a)        -> ps "  lb %s, %s" (fmt_reg r) (fmt_loc a)
  | Move (rd, rs)    -> ps "  move %s, %s" (fmt_reg rd) (fmt_reg rs)
  | Addi (rd, rs, i) -> ps "  addi %s, %s, %d" (fmt_reg rd) (fmt_reg rs) i
  | Add (rd, rs, rt) -> ps "  add %s, %s, %s" (fmt_reg rd) (fmt_reg rs) (fmt_reg rt)
  | Mul (rd, rs, rt) -> ps "  mul %s, %s, %s" (fmt_reg rd) (fmt_reg rs) (fmt_reg rt)
  | Sub (rd, rs, rt) -> ps "  sub %s, %s, %s" (fmt_reg rd) (fmt_reg rs) (fmt_reg rt)
  | Div (rd, rs, rt) -> ps "  div %s, %s, %s" (fmt_reg rd) (fmt_reg rs) (fmt_reg rt)
  | Mod (rd, rs, rt) -> ps "  div %s, %s, %s" (fmt_reg rd) (fmt_reg rs) (fmt_reg rt)
  | Equal (rd, rs, rt) -> ps "  div %s, %s, %s" (fmt_reg rd) (fmt_reg rs) (fmt_reg rt)
  | And (rd, rs, rt) -> ps "  div %s, %s, %s" (fmt_reg rd) (fmt_reg rs) (fmt_reg rt)
  | Or (rd, rs, rt) -> ps "  div %s, %s, %s" (fmt_reg rd) (fmt_reg rs) (fmt_reg rt)
  | Nequal (rd, rs, rt) -> ps "  div %s, %s, %s" (fmt_reg rd) (fmt_reg rs) (fmt_reg rt)
  | Inf (rd, rs, rt) -> ps "  div %s, %s, %s" (fmt_reg rd) (fmt_reg rs) (fmt_reg rt)
  | Sup (rd, rs, rt) -> ps "  div %s, %s, %s" (fmt_reg rd) (fmt_reg rs) (fmt_reg rt)
  | Infeq (rd, rs, rt) -> ps "  div %s, %s, %s" (fmt_reg rd) (fmt_reg rs) (fmt_reg rt)
  | Supeq (rd, rs, rt) -> ps "  div %s, %s, %s" (fmt_reg rd) (fmt_reg rs) (fmt_reg rt)
  | Syscall          -> ps "  syscall"
  | B (l)            -> ps "  b %s" l
  | Beqz (r, l)      -> ps "  beqz %s, %s" (fmt_reg r) l
  | Jal (l)          -> ps "  jal %s" l
  | Jr (r)           -> ps "  jr %s" (fmt_reg r)

let fmt_dir = function
  | Asciiz (s) -> ps ".asciiz \"%s\"" s

let emit oc asm =
  Printf.fprintf oc ".text\n.globl main\n" ;
  List.iter (fun i -> Printf.fprintf oc "%s\n" (fmt_instr i)) asm.text ;
  Printf.fprintf oc "\n.data\n" ;
  List.iter (fun (l, d) -> Printf.fprintf oc "%s: %s\n" l (fmt_dir d)) asm.data
