(*
 * puref - 
 * Copyright (C) 2010 nineties
 * 
 * $Id: lexer.mll 2010-06-27 23:08:49 nineties $
 *)

{
(* open Parser *)
type token =
      Tvar of string
    | Tint of int
    | Tlet
    | Tletrec
    | Tin
    | Tcase
    | Tof
    | Tpack
    | Tlbrace
    | Trbrace
    | Tlparen
    | Trparen
    | Tbackslash
    | Tsemi
    | Tequal
    | Tplus
    | Tminus
    | Tmul
    | Tdiv
    | Tless
    | Tlessequal
    | Tequalequal
    | Tnotequal
    | Tgreaterequal
    | Tgreater
    | Tand
    | Tor
    | Tarrow

}

let space  = [' ' '\t' '\n' '\r']
let letter = ['A'-'Z' 'a'-'z' '_']
let digit  = ['0'-'9']

rule main = parse
    space +
    { main lexbuf }
    | ';' [^ '\n']* (* comment *)
    { main lexbuf }
    | letter (letter | digit)*
    { match Lexing.lexeme lexbuf with
          "let" -> Tlet
        | "letrec" -> Tletrec
        | "in" -> Tin
        | "case" -> Tcase
        | "of" -> Tof
        | "Pack" -> Tpack
        | s -> Tvar s }
    | '{' { Tlbrace }
    | '}' { Trbrace }
    | '(' { Tlparen }
    | ')' { Trparen }
    | '\\' { Tbackslash }
    | ';' { Tsemi }
    | '=' { Tequal }
    | '+' { Tplus }
    | '-' { Tminus }
    | '*' { Tmul }
    | '/' { Tdiv }
    | '<' { Tless }
    | "<=" { Tlessequal }
    | "==" { Tequalequal }
    | "~=" { Tnotequal }
    | ">=" { Tgreaterequal }
    | '>' { Tgreater }
    | '&' { Tand }
    | '|' { Tor }
    | "->" { Tarrow }

