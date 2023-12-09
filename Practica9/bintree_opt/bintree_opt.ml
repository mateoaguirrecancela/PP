(*
open Bintree

let is_bst cmp tree =
  let rec is_bst_helper node min_bound max_bound =
    match node with
    | Empty -> true
    | Node (left, value, right) ->
        cmp min_bound value && cmp value max_bound
        && is_bst_helper left min_bound value
        && is_bst_helper right value max_bound
  in
  is_bst_helper tree min_bound_of_cmp max_bound_of_cmp

let bfs tree =
  let rec bfs' queue acc =
    match queue with
    | [] -> List.rev acc
    | Empty :: tl -> bfs' tl acc
    | Node (left, value, right) :: tl ->
        let new_queue = tl @ [left; right] in
        bfs' new_queue (value :: acc)
  in
  bfs' [tree] []

let bfs' tree =
  let rec bfs'' queue acc =
    match queue with
    | [] -> List.rev acc
    | Empty :: tl -> bfs'' tl acc
    | Node (left, value, right) :: tl ->
        let new_queue = tl @ [left; right] in
        bfs'' new_queue (value :: acc)
  in
  bfs'' [tree] []

let perfecto tree =
  let rec height t =
    match t with
    | Empty -> 0
    | Node (left, _, right) -> 1 + max (height left) (height right)
  in
  let rec node_count t h =
    match t with
    | Empty -> 0
    | Node (left, _, right) when h = 1 -> 1
    | Node (left, _, right) -> node_count left (h - 1) + node_count right (h - 1)
  in
  let h = height tree in
  let n = node_count tree h in
  n = int_of_float (2. ** float_of_int h) - 1

let casi_completo tree =
  let rec level_check level nodes =
    match nodes with
    | [] -> true
    | Empty :: tl -> level_check (level - 1) tl
    | Node (left, _, right) :: tl when level > 0 ->
        level_check (level - 1) (left :: right :: tl)
    | Node (_, _, _) :: _ -> level_check (level - 1) tl
  in
  let rec last_level_check level nodes =
    match nodes with
    | [] -> true
    | Empty :: tl -> last_level_check (level - 1) tl
    | Node (left, _, right) :: tl when level > 1 ->
        last_level_check (level - 1) (left :: right :: tl)
    | Node (left, _, right) :: tl -> left = Empty && right = Empty
  in
  let rec check_all_levels tree =
    let rec level_nodes level queue =
      match queue with
      | [] -> []
      | Empty :: tl -> Empty :: level_nodes level tl
      | Node (left, _, right) :: tl ->
          Node (left, 0, right) :: level_nodes level tl
    in
    let level = level_of_tree tree in
    let level_nodes = level_nodes level [tree] in
    if level_check level level_nodes then last_level_check level level_nodes
    else false
  in
  check_all_levels tree
*)

(* bintree_opt.ml *)

open Bintree

(* Función is_bst para verificar si un árbol es de búsqueda *)
let is_bst ord tree =
  let rec aux min_val max_val = function
    | Empty -> true
    | Node (value, left, right) ->
      ord min_val value && ord value max_val &&
      aux min_val value left && aux value max_val right
  in
  aux min_int max_int tree

(* Función bfs para el recorrido en anchura del árbol *)
let bfs tree =
  let rec aux queue acc =
    match queue with
    | [] -> List.rev acc
    | Empty :: rest -> aux rest acc
    | Node (value, left, right) :: rest ->
      aux (rest @ [left; right]) (value :: acc)
  in
  aux [tree] []

(* Función bfs' para el recorrido en anchura de manera recursiva terminal *)
let bfs' tree =
  let rec aux queue acc =
    match queue with
    | [] -> List.rev acc
    | Empty :: rest -> aux rest acc
    | Node (value, left, right) :: rest ->
      aux (rest @ [left; right]) (value :: acc)
  in
  aux [tree] []

(* Función perfecto para verificar si un árbol es perfecto *)
let perfecto tree =
  let rec altura = function
    | Empty -> 0
    | Node (_, left, _) -> 1 + altura left
  in
  let rec aux h = function
    | Empty -> h = 0
    | Node (_, left, right) ->
      altura left = altura right &&
      aux (h - 1) left &&
      aux (h - 1) right
  in
  aux (altura tree) tree

(* Función casi_completo para verificar si un árbol es casi completo *)
let casi_completo tree =
  let rec aux nivel = function
    | Empty -> nivel = 0
    | Node (_, left, right) ->
      (aux (nivel - 1) left && aux (nivel - 1) right) ||
      (nivel = 1 && left = Empty && right = Empty)
  in
  aux (altura tree) tree
