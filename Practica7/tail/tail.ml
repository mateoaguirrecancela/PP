let to0from n =
  let rec to0from_aux acc n =
    if n < 0 then acc
    else to0from_aux (n :: acc) (n - 1)
  in
  to0from_aux [] n

let fromto m n =
  let rec fromto_aux acc m n =
    if m > n then acc
    else fromto_aux (m :: acc) (m + 1) n
  in
  fromto_aux [] m n

let remove x lst =
  List.fold_right
    (fun h t -> if x = h then t else h :: t)
    lst []

let compress lst =
  let rec compress_aux acc = function
    | h1 :: h2 :: t ->
        if h1 = h2 then compress_aux acc (h2 :: t)
        else compress_aux (h1 :: acc) (h2 :: t)
    | l -> List.rev_append l acc
  in
  compress_aux [] lst

let append' = List.rev_append

let map' f lst =
  let rec map_aux acc = function
    | [] -> List.rev acc
    | h :: t -> map_aux (f h :: acc) t
  in
  map_aux [] lst

let fold_right' f lst init =
  List.fold_left (fun acc x -> f x acc) init (List.rev lst)

let incseg lst =
  List.fold_left
    (fun acc x -> List.rev_append (List.rev_map ((+) x) acc) (x :: []))
    [] lst
