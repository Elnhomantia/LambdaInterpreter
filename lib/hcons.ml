module Hcons = struct

  type 'a hconsType = {node: 'a; id: int};;

  type terme = termeNode hconsType
  and termeNode = Var of int | Lambda of terme | App of terme * terme;;

  module TermNodeHash = struct
    type t = termeNode
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