(* bintree_opt.ml *)

open Bintree

let is_bst ord tree =
  let rec is_bst_helper min_value max_value = function
    | Empty -> true
    | Node (value, left, right) ->
        ord min_value value && ord value max_value
        && is_bst_helper min_value value left
        && is_bst_helper value max_value right
  in
  is_bst_helper min_int max_int tree

let bfs tree =
  let rec bfs_helper queue acc =
    match queue with
    | [] -> List.rev acc
    | Empty :: tl -> bfs_helper tl acc
    | Node (value, left, right) :: tl ->
        let new_queue = tl @ [left; right] in
        bfs_helper new_queue (value :: acc)
  in
  bfs_helper [tree] []

let bfs' tree =
  let rec bfs_helper queue acc =
    match queue with
    | [] -> List.rev acc
    | Empty :: tl -> bfs_helper tl acc
    | Node (value, left, right) :: tl ->
        let new_queue = tl @ [left; right] in
        bfs_helper new_queue (value :: acc)
  in
  bfs_helper [tree] []

let perfecto tree =
  let rec perfecto_helper depth = function
    | Empty -> depth = 0
    | Node (_, left, right) ->
        perfecto_helper (depth - 1) left && perfecto_helper (depth - 1) right
  in
  let rec depth = function
    | Empty -> 0
    | Node (_, left, _) -> 1 + depth left
  in
  perfecto_helper (depth tree) tree

let casi_completo tree =
  let rec count_nodes = function
    | Empty -> 0
    | Node (_, left, right) -> 1 + count_nodes left + count_nodes right
  in
  let rec casi_completo_helper depth = function
    | Empty -> depth = 0
    | Node (_, left, right) ->
        casi_completo_helper (depth - 1) left && casi_completo_helper (depth - 1) right
  in
  let rec depth = function
    | Empty -> 0
    | Node (_, left, _) -> 1 + depth left
  in
  let node_count = count_nodes tree in
  let max_depth = depth tree in
  (pow 2 max_depth - 1) <= node_count && node_count < pow 2 (max_depth + 1)

(* Exponential function for integer powers *)
let rec pow base exp =
  if exp = 0 then 1
  else base * pow base (exp - 1)
