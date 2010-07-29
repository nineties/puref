(*
 * puref - 
 * Copyright (C) 2010 nineties
 * 
 * $Id: compile.ml 2010-07-29 19:48:29 nineties $
 *)

open Syntax
open Gmachine

let argpos name args = 
    let rec f i ls = match ls with
        | [] -> failwith ("unknown variable: " ^ name)
        | v::_ when name = v -> i
        | _ -> f (i+1) (List.tl ls)
    in f 0 args

let rec compileC base args = function
    | VarE name when List.mem name args -> [PushI (base + argpos name args)]
    | VarE name -> [ScI name]
    | NumE num -> [NumI num]
    | AppE(e1,e2) -> compileC base args e2 @ compileC (base + 1) args e1 @ [MkappI]
    | _ -> failwith "not implemented"

let compileR args body =
    let narg = List.length args in
    compileC 0 args body @ [UpdateI narg; PopI narg; UnwindI]

let compileSC (name,args,body) =
    (name, List.length args, compileR args body)

let f prog = List.map compileSC prog
