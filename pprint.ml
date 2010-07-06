(*
 * puref - 
 * Copyright (C) 2010 nineties
 * 
 * $Id: pprint.ml 2010-07-07 02:28:51 nineties $
 *)

open Format
open Syntax

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
    | PackE (id,arity)    -> fprintf ppf "<%d,%d>" id arity
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
    = fprintf ppf "@[<%d> %a-> %a@]" id pp_vars defs pp_expr cont

let pp_sc ppf (vars,body)
    = fprintf ppf "@[%a= %a@]" pp_vars vars pp_expr body

let rec pp_program ppf = function
    | [sc] -> pp_sc ppf sc; pp_print_newline ppf ()
    | sc::scs -> fprintf ppf "@[<v>%a;@;%a@]" pp_sc sc pp_program scs
    | _ -> failwith "not reachable"

