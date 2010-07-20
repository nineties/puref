/*
 * puref - 
 * Copyright (C) 2010 nineties
 * 
 * $Id: parser.mly 2010-07-20 21:25:53 nineties $
 */

%{
open Syntax
%}

%token <string> Tvar
%token <int> Tint
%token Tlet Tletrec Tin Tcase Tof Tpack Tarrow Tlbrace Trbrace Tlparen Trparen
    Tbackslash Tsemi Tequal Tplus Tminus Tmul Tdiv Tless Tlessequal Tequalequal
    Tnotequal Tgreaterequal Tgreater Tand Tor Tcomma Tdot Teof

%right	  Tor
%right 	  Tand
%nonassoc Tminus
%right 	  Tplus
%nonassoc Tdiv
%right    Tmul
%left 	  prec_app

%start program
%type <Syntax.supercombinator list> program

%%

program:
      sc Teof          { [$1] }
    | sc Tsemi program { $1::$3 }
;
sc: // super combinator
    vars Tequal expr { (List.hd $1, List.tl $1, $3) }
;
expr:
      expr aexpr %prec prec_app { AppE($1,$2) }
    | expr binop expr           { InfixE($2,$1,$3) }
    | Tlet defns Tin expr       { LetE($2,$4) }
    | Tletrec defns Tin expr    { LetrecE($2,$4) }
    | Tcase expr Tof alts       { CaseE($2,$4) }
    | Tbackslash vars Tdot expr { LambdaE($2,$4) }
    | aexpr                     { $1 }
;
aexpr: // atomic expression
      Tvar      { VarE($1) }
    | Tint      { NumE($1) }
    | Tpack Tlbrace Tint Tcomma Tint Trbrace { PackE($3,$5) }
    | Tlparen expr Trparen { $2 }
;
defns: // definitions
      defn              { [$1] }
    | defn Tsemi defns  { $1::$3 }
;
defn: // definition
    Tvar Tequal expr { ($1,$3) }
;
alts: // alternatives
      alt            { [$1] }
    | alt Tsemi alts { $1::$3 }
;
alt: // alternative
    Tless Tint Tgreater vars Tarrow expr { ($2,$4,$6) }
;
binop:
      Tplus         { Add }
    | Tminus        { Sub }
    | Tmul          { Mul }
    | Tdiv          { Div }
    | Tless         { Lt }
    | Tlessequal    { Le }
    | Tequalequal   { Eq }
    | Tnotequal     { Ne }
    | Tgreaterequal { Ge }
    | Tgreater      { Gt }
    | Tand          { And }
    | Tor           { Or }
;
vars:
      Tvar      { [$1] }
    | Tvar vars { $1::$2 }
;
%%
