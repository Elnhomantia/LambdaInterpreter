open Hcons.Hcons

type console =
|U of unit
|T of term

type reduction_strategy = 
| LoAll
| RiAll
| LoExcl
| RiExcl

type command =
| Next of reduction_strategy * command
| AllNext of reduction_strategy * command
| Normalise of command
| Term of term

module TermNodeHash = struct
  type t = {name:string; value:term Stack.t}
  let equal x y = x.name = y.name
  let hash x = Hashtbl.hash x.name
end

module TermNodeTable = Hashtbl.Make(TermNodeHash)
let varTable : TermNodeHash.t TermNodeTable.t = TermNodeTable.create 1024

(**

*)
let addNvar (name:string) (value:term) =
  let dummy_stack = Stack.create () in
  let key = { TermNodeHash.name = name; value = dummy_stack } in
  try
    let entry = TermNodeTable.find varTable key in
    Stack.push value entry.value
  with Not_found ->
    let new_stack = Stack.create () in
    Stack.push value new_stack;
    let entry = { TermNodeHash.name = name; value = new_stack } in
    TermNodeTable.add varTable entry entry

(**

*)
let getNvar (name:string) :term =
  let dummy_stack = Stack.create () in (*Needed to search with full record ???*)
  let key = { TermNodeHash.name = name; value = dummy_stack } in
  try
    let entry = TermNodeTable.find varTable key in
    Stack.top entry.value
  with
  | Not_found -> failwith ("(GET) Variable " ^ name ^ " does not exist.")
  | Stack.Empty -> failwith ("(GET) No value associated with " ^ name)

(**
  If I ever want to do env blocks
*)
let popNvar (name : string) : term =
  let dummy_stack = Stack.create () in
  let key = { TermNodeHash.name = name; value = dummy_stack } in
  try
    let entry = TermNodeTable.find varTable key in
    if Stack.is_empty entry.value then
      failwith ("(POP) No value associated with " ^ name)
    else
      Stack.pop entry.value
  with
  | Not_found -> failwith ("(POP) Variable " ^ name ^ " does not exist.")

(**

*)
let rec increment (offset:int)(cutoff:int) (t:term) :term=
  match t.node with
  | Var v -> if v >= cutoff then sVar(v + offset) else sVar(v)
  | Lambda l -> sLambda(increment offset (cutoff + 1) l)
  | App(t1, t2) -> sApp (increment offset cutoff t1) (increment offset cutoff t2)

(**
  t : the term in which the substitution take place
  target : the substitute
  depth : index of De Bruijn
*)
let rec substitue (t:term) (target:term) (depth:int) :term=
  match t.node with
  | Var v -> 
    if v = depth then (increment depth 0 target)
    else sVar (v)
  | Lambda l -> sLambda (substitue l target (depth + 1))
  | App(t1, t2) -> sApp (substitue t1 target depth) (substitue t2 target depth)

(**
  Left outer beta reduction, go through Lambdas
  t: term to reduce
  Return the term after one step of beta reduction
  Return the term itself if not possible
*)
let rec loAll (t:term) =
  match t.node with
  |Lambda l -> sLambda (loAll l)  
  |Var _ -> t
  |App(t1, t2) -> (*outer left*)
    match t1.node with
    |Lambda l ->
      let t2_inc = increment 1 0 t2 in (*Increment so it doesn't collide*)
      let l_sub = substitue l t2_inc 0 in (*We replace*)
      increment (-1) 0 l_sub (*We removed a lambda, so -1*)
    |_ ->
      let t1' = loAll t1 in
      if t1' != t1 then sApp t1' t2
      else 
        let t2' = loAll t2 in
        if t2' != t2 then sApp t1 t2'
        else t


(** TODO
  Right inner beta reduction, go through Lambdas
  t: term to reduce
  Return the term after one step of beta reduction
  Return the term itself if not possible
*)
let rec riAll (t:term) =
  match t.node with
  |Var _ -> t
  |Lambda l -> sLambda(riAll l)
  |App(t1, t2) -> (*We first go all the way down*)
    let t2' = riAll t2 in
    if t2' != t2 then sApp t1 t2'
    else
      let t1' = riAll t1 in
      if t1' != t1 then sApp t1' t2
      else
        match t1.node with (*Then we can reduce if possible*)
        |Lambda l -> 
          let t2_inc = increment 1 0 t2 in (*Increment so it doesn't collide*)
          let l_sub = substitue l t2_inc 0 in (*We replace*)
          increment (-1) 0 l_sub (*We removed a lambda, so -1*)
        |_ -> t

(** TODO
    
*)
let rec loExcl (t:term) = 
  match t.node with
  |Lambda _ -> t (*We don't go under Lambdas*)
  |Var _ -> t
  |App(t1, t2) -> (*outer left*)
    match t1.node with
    |Lambda l ->
      let t2_inc = increment 1 0 t2 in (*Increment so it doesn't collide*)
      let l_sub = substitue l t2_inc 0 in (*We replace*)
      increment (-1) 0 l_sub (*We removed a lambda, so -1*)
    |_ ->
      let t1' = loExcl t1 in
      if t1' != t1 then sApp t1' t2
      else 
        let t2' = loExcl t2 in
        if t2' != t2 then sApp t1 t2'
        else t

(** TODO
    Beta reduction with no reduction under lambdas
*)
let riExcl (t:term) =
  match t.node with
  |Var _ -> t
  |Lambda _ -> t
  |App(t1, t2) -> (*We first go all the way down*)
    let t2' = riAll t2 in
    if t2' != t2 then sApp t1 t2'
    else
      let t1' = riAll t1 in
      if t1' != t1 then sApp t1' t2
      else
        match t1.node with (*Then we can reduce if possible*)
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
  if t' != t then t
  else normalise strategy t'


let rec string_of_terme t =
  match t.node with
  |Var v -> string_of_int v
  |Lambda l -> "L." ^ string_of_terme l
  |App (t1, t2) -> "(" ^ string_of_terme t1 ^ " " ^ string_of_terme t2 ^ ")"