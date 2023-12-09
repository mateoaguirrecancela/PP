type 'a bintree = Empty | Node of 'a * 'a bintree * 'a bintree

(*recorrer el arbol en orden*)
let rec in_order tree =
  match tree with
  | Empty -> []
  | Node (value, left, right) -> in_order left @ [value] @ in_order right

(*insertar un elemento en un arbol de busqueda*)
let rec insert ord tree x =
  match tree with
  | Empty -> Node (x, Empty, Empty)
  | Node (value, left, right) ->
    if ord x value then
      Node (value, insert ord left x, right)
    else
      Node (value, left, insert ord right x)

(*construir un arbol de busqueda a partir de una lista*)
let bst ord lst =
  List.fold_left (fun acc x -> insert ord acc x) Empty lst

(*ordenar una lista utilizando quicksort*)
let rec qsort ord lst =
  match lst with
  | [] -> []
  | pivot :: rest ->
    let lesser, greater = List.partition (fun x -> ord x pivot) rest in
    qsort ord lesser @ [pivot] @ qsort ord greater
