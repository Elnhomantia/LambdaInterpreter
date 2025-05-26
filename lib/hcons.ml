module Hcons = struct

  type 'a hconsType = {node: 'a; id: int};;

  type term = termNode hconsType
  and termNode = Var of int | Lambda of term | App of term * term;;

  module TermNodeHash = struct
    type t = termNode
    let equal x y = x = y
    let hash = Hashtbl.hash
  end

  module TermNodeTable = Hashtbl.Make(TermNodeHash)

  let hcons  =
  let table = TermNodeTable.create 1024 
  and get_id = ref 0 in 
  fun node ->

  try 
    TermNodeTable.find table node
  with Not_found ->
    let id = !get_id in
    incr get_id;
    let hcons_node = { node; id } in
    TermNodeTable.add table node hcons_node;
    hcons_node

  let sVar (x:int) = hcons(Var(x))
  let sLambda t = hcons(Lambda(t))
  let sApp t1 t2 = hcons(App(t1, t2))
end