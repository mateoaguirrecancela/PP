open Bintree

let rec is_bst_helper compare_min_max tree =
  match tree with
  | Empty -> true
  | Node (value, left, right) ->
    let left_max, right_min = compare_min_max value in
    is_bst_helper (fun x -> (x, right_min)) left &&
    is_bst_helper (fun x -> (left_max, x)) right

let is_bst ord tree =
  let compare_min_max value = (value, value) in
  is_bst_helper compare_min_max tree

let bfs tree =
  let rec bfs_helper queue acc =
    match queue with
    | [] -> List.rev acc
    | Empty :: rest -> bfs_helper rest acc
    | Node (value, left, right) :: rest ->
      bfs_helper (rest @ [left; right]) (acc @ [value])
  in
  bfs_helper [tree] []

let bfs' tree =
  let rec bfs'_helper queue acc =
    match queue with
    | [] -> List.rev acc
    | Empty :: rest -> bfs'_helper rest acc
    | Node (value, left, right) :: rest ->
      bfs'_helper (rest @ [left; right]) (acc @ [value])
  in
  bfs'_helper [tree] []

let rec depth tree =
  match tree with
  | Empty -> 0
  | Node (_, left, right) -> 1 + max (depth left) (depth right)

let perfecto tree =
  let rec is_perfect depth tree =
    match tree with
    | Empty -> depth = 0
    | Node (_, left, right) ->
      is_perfect (depth - 1) left && is_perfect (depth - 1) right
  in
  is_perfect (depth tree) tree

let casi_completo tree =
  let rec is_almost_complete depth tree =
    match tree with
    | Empty -> depth <= 1
    | Node (_, left, right) ->
      is_almost_complete (depth - 1) left && is_almost_complete (depth - 1) right
  in
  is_almost_complete (depth tree) tree
