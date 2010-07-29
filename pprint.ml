(*
 * puref - 
 * Copyright (C) 2010 nineties
 * 
 * $Id: pprint.ml 2010-07-30 03:56:18 nineties $
 *)

open Format
open Syntax
open Vmtypes

let binop_string = function
     | Add -> "+"  | Sub -> "-"  | Mul -> "*"  | Div -> "/"  | Lt  -> "<"
     | Le  -> "<=" | Eq  -> "==" | Ne  -> "~=" | Ge  -> ">=" | Gt  -> ">"
     | And -> "&"  | Or  -> "    | "

let rec pp_vars ppf vars
    = List.iter (fun v -> fprintf ppf "%a@;" pp_print_string v) vars

let pp_break_list f ppf elems = 
    pp_open_hvbox ppf 0;
    f ppf (List.hd elems);
    List.iter (fun def -> pp_print_break ppf 1 0; f ppf def) (List.tl elems);
    pp_close_box ppf ()

let rec pp_expr ppf = function
    | VarE id             -> pp_print_string ppf id
    | NumE num            -> pp_print_int ppf num
    | PackE (id,arity)    -> fprintf ppf "Pack{%d,%d}" id arity
    | AppE (f,arg)        -> fprintf ppf "%a %a" pp_expr f pp_aexpr arg
    | InfixE (op,lhs,rhs)
    -> fprintf ppf "@[%a %s %a@]" pp_expr lhs (binop_string op) pp_expr rhs
    | LetE (defs,cont)
    -> fprintf ppf "@[@[<hv 4>let %a@]@;in@;%a@]" (pp_break_list pp_def) defs pp_expr cont
    | LetrecE (defs,cont)
    -> fprintf ppf "@[@[<hv 4>letrec@;%a@]@;in@;%a@]" (pp_break_list pp_def) defs pp_expr cont
    | CaseE (expr,alts)
    -> fprintf ppf "@[@[<hv 4>case %a@]@;of@;%a@]" pp_expr expr (pp_break_list pp_alt) alts
    | LambdaE (vars,body)
    -> fprintf ppf "@[@[<hv>%a>. %a@]" pp_vars vars pp_expr body

and pp_aexpr ppf exp = match exp with
    | VarE _ -> pp_expr ppf exp
    | NumE _ -> pp_expr ppf exp
    | PackE _ -> pp_expr ppf exp
    | _ -> fprintf ppf "(%a)" pp_expr exp

and pp_def ppf (var,expr)
    = fprintf ppf "@[%a = %a@]" pp_print_string var pp_expr expr

and pp_alt ppf (id,elems,cont)
    = fprintf ppf "@[<%d> %a-> %a@]" id pp_vars elems pp_expr cont

let pp_sc ppf (var,vars,body)
    = fprintf ppf "@[%s %a= %a@]" var pp_vars vars pp_expr body

let rec parsed ppf = function
    | [sc] -> pp_sc ppf sc; pp_print_newline ppf ()
    | sc::scs -> fprintf ppf "@[<v>%a;@;%a@]" pp_sc sc parsed scs
    | _ -> failwith "not reachable"

let pp_insn ppf = function
    | ScI name  -> fprintf ppf "SC %s@." name
    | NumI num  -> fprintf ppf "Num %d@." num
    | MkappI    -> fprintf ppf "Mkapp@."
    | PushI i   -> fprintf ppf "Push %d@." i
    | SlideI n  -> fprintf ppf "Slide %d@." n
    | UpdateI n -> fprintf ppf "Update %d@." n
    | PopI n    -> fprintf ppf "Pop %d@." n
    | UnwindI   -> fprintf ppf "Unwind@."

let rec compiled ppf = function
    | [] -> ()
    | (name,narg,body)::scs -> begin
        fprintf ppf "=== %s [%d] ===@." name narg;
        ignore(List.fold_left (fun i insn -> 
            fprintf ppf "%4d: " i; pp_insn ppf insn; i+1) 1 body);
        compiled ppf scs
    end
