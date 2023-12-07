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
