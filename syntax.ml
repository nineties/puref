(*
 * puref - 
 * Copyright (C) 2010 nineties
 * 
 * $Id: syntax.ml 2010-06-28 00:05:47 nineties $
 *)

type variable = string
type binaryop = Add|Sub|Mul|Div|Lt|Le|Eq|Ne|Ge|Gt|And|Or
type definition = variable * expression
and alternative = int * variable list * expression
and expression =
      VarE of variable
    | NumE of int
    | PackE of int * int
    | AppE of expression * expression
    | InfixE of binaryop * expression * expression
    | LetE of definition list * expression
    | LetrecE of definition list * expression
    | CaseE of expression * alternative list
    | LambdaE of variable list * expression

type supercombinator = variable list * expression
