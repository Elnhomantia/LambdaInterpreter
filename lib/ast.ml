open Hcons.Hcons

type reduction_strategy = 
| LoAll
| RiAll
| LoExcl
| RiExcl

type term =
| Var of int
| Lambda of term
| App of term * term

type termList =
| Empty
| Node of term * termList

type command =
| Next of reduction_strategy * command
| AllNext of reduction_strategy * command
| Normalise of command
| Term of term

type var =
| Unvar of int
| Nvar of string

(**

*)
let rec increment (offset:int)(cutoff:int)(t:term) :term=
  match t with
  | Var v -> if v >= cutoff then Var(v + offset) else Var(v)
  | Lambda l -> Lambda(increment offset (cutoff + 1) l)
  | App(t1, t2) -> App(increment offset cutoff t1, increment offset cutoff t2)

(**
  t : the term in which the substitution take place
  target : the substitute
  depth : index of De Bruijn
*)
let rec substitue (t:term) (target:term) (depth:int) =
  match t with
  | Var v -> 
    if v = depth then (increment depth 0 target)
    else Var(v)
  | Lambda l -> Lambda(substitue l target (depth + 1))
  | App(t1, t2) -> App(substitue t1 target depth, substitue t2 target depth)

(**
  Left outer beta reduction, go through Lambdas
  t: term to reduce
  Return the term after one step of beta reduction
  Return the term itself if not possible
*)
let rec loAll (t:term) =
  match t with
  |Var _ -> t
  |App (Lambda l, t1) ->
    let t1_inc = increment 1 0 t1 in (*Increment so it doesn't collide*)
    let l_sub = substitue l t1_inc 0 in (*We replace*)
    increment (-1) 0 l_sub (*We removed a lambda, so -1*)
  |App(t1, t2) -> (*outer left*)
    let t1' = loAll t1 in
    if t1' <> t1 then App(t1', t2)
    else 
      let t2' = loAll t2 in
      if t2' <> t2 then App(t1, t2')
      else t
  |Lambda l -> Lambda(loAll l)

(** TODO
  Right inner beta reduction, go through Lambdas
  t: term to reduce
  Return the term after one step of beta reduction
  Return the term itself if not possible
*)
let rec riAll (t:term) =
  match t with
  |Var _ -> t
  |Lambda l -> Lambda(riAll l)
  |App(t1, t2) -> (*We first go all the way down*)
    let t2' = riAll t2 in
    if t2' <> t2 then App(t1, t2')
    else
      let t1' = riAll t1 in
      if t1' <> t1 then App(t1', t2)
      else
        match t1 with (*Then we can reduce if possible*)
        |Lambda l -> 
          let t2_inc = increment 1 0 t2 in (*Increment so it doesn't collide*)
          let l_sub = substitue l t2_inc 0 in (*We replace*)
          increment (-1) 0 l_sub (*We removed a lambda, so -1*)
        |_ -> t

(** TODO
    
*)
let rec loExcl (t:term) = 
  match t with
  |Var _ -> t
  |App (Lambda l, t1) ->
    let t1_inc = increment 1 0 t1 in (*Increment so it doesn't collide*)
    let l_sub = substitue l t1_inc 0 in (*We replace*)
    increment (-1) 0 l_sub (*We removed a lambda, so -1*)
  |App(t1, t2) -> (*outer left*)
    let t1' = loExcl t1 in
    if t1' <> t1 then App(t1', t2)
    else 
      let t2' = loExcl t2 in
      if t2' <> t2 then App(t1, t2')
      else t
  |Lambda _ -> t (*We don't go under Lambdas*)

(** TODO
    Beta reduction with no reduction under lambdas
*)
let riExcl (t:term) =
  match t with
  |Var _ -> t
  |Lambda _ -> t
  |App(t1, t2) -> (*We first go all the way down*)
    let t2' = riAll t2 in
    if t2' <> t2 then App(t1, t2')
    else
      let t1' = riAll t1 in
      if t1' <> t1 then App(t1', t2)
      else
        match t1 with (*Then we can reduce if possible*)
        |Lambda l -> 
          let t2_inc = increment 1 0 t2 in (*Increment so it doesn't collide*)
          let l_sub = substitue l t2_inc 0 in (*We replace*)
          increment (-1) 0 l_sub (*We removed a lambda, so -1*)
        |_ -> t

(**
  Manage strategy for "next" instructions
*)
let betaReduction (strategy:reduction_strategy) (t:term) :term=
  match strategy with
  |LoAll -> loAll t
  |RiAll -> riAll t
  |LoExcl -> loExcl t
  |RiExcl -> riExcl t

(**
  TODO
*)
let betaReductionAll (strategy:reduction_strategy) (t:term) :term=
  match strategy with
  |LoAll -> t
  |RiAll -> t
  |LoExcl -> t
  |RiExcl -> t

(**
  TODO
*)
let rec normalise (strategy:reduction_strategy) (t:term) :term= 
  let norm = (*Strategy handler*)
    match strategy with
      |LoAll -> loAll
      |RiAll -> riAll
      |LoExcl -> loExcl
      |RiExcl -> riExcl
  in
  let t' = norm t in (*While we can reduce, we do*)
  if t' <> t then t
  else normalise strategy t'


let rec string_of_terme t =
  match t with
  |Var v -> string_of_int v
  |Lambda l -> "L." ^ string_of_terme l
  |App (t1, t2) -> "(" ^ string_of_terme t1 ^ " " ^ string_of_terme t2 ^ ")"