
type pos = {line : int ; col: int}

type span = Span of pos * pos

type 'a loc = Loc of span * 'a

type var = string

type base_type = Int | Bool | Unit | List of base_type loc

type relsort = RelSort of base_type loc list

type op = Plus | Minus | Times | LTE

type ty = Arrow of ty loc * ty loc
        | Refty of var loc * base_type loc * pred loc
and synth = Annot of check loc  * ty loc
          | App of synth loc* check loc
          | Var of var loc
          | Lit of int loc
          | True
          | False
          | Arith of op * check loc* check loc
          | Unitcon
and check = S of synth
          | Lam of var loc * check loc
          | Nil
          | Cons of check loc * check loc
          | Reclist of synth loc * check loc* check loc
          | If of check loc * check loc * check loc

and reln = Rapp of var * check
         | Star of reln
         | Prod of reln * reln
         | Union of reln * reln
         | Litreln of synth list list

and pred = Eq of reln *  reln
         | SubsetEq of reln * reln
         | And of pred * pred
         | Or of pred * pred
         | Yep | Nope


type decl = Reldec of var loc * base_type loc * relsort loc
          | Def of var loc * synth loc

type prog =  Prog of  decl list  * synth loc
