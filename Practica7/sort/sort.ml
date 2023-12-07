(* sort.ml *)
open Random

let bigl =
  let rec aux acc n =
    if n = 0 then acc else aux (n :: acc) (n - 1)
  in
  aux [] 10000

let rec insert_t x lst =
  match lst with
  | [] -> [x]
  | h :: t -> if x <= h then x :: h :: t else h :: insert_t x t

let rec isort_t lst =
  match lst with
  | [] -> []
  | h :: t -> insert_t h (isort_t t)

let rec rlist n =
  let rec aux acc n =
    if n = 0 then acc else aux (Random.int 1000 :: acc) (n - 1)
  in
  aux [] n

let lc1 = List.init 10000 (fun x -> x)

let lc2 = List.init 20000 (fun x -> x)

let ld1 = List.rev lc1

let ld2 = List.rev lc2

let lr1 = rlist 10000

let lr2 = rlist 20000

let crono f x =
  let t = Sys.time () in
  let _ = f x in
  Sys.time () -. t

let isort_g ord lst =
  let rec insert_g x lst =
    match lst with
    | [] -> [x]
    | h :: t -> if ord x h then x :: h :: t else h :: insert_g x t
  in
  let rec isort_helper acc = function
    | [] -> acc
    | h :: t -> isort_helper (insert_g h acc) t
  in
  isort_helper [] lst

let bigl2 =
  let rec aux acc n =
    if n = 0 then acc else aux (n :: acc) (n - 1)
  in
  aux [] 100000

let rec split_t lst =
  let rec split_helper acc n lst =
    if n = 0 then (List.rev acc, lst)
    else
      match lst with
      | [] -> (List.rev acc, [])
      | h :: t -> split_helper (h :: acc) (n - 1) t
  in
  split_helper [] (List.length lst / 2) lst

let rec merge_t (lst1, lst2) =
  match (lst1, lst2) with
  | [], lst | lst, [] -> lst
  | h1 :: t1, h2 :: t2 -> if h1 <= h2 then h1 :: merge_t (t1, lst2) else h2 :: merge_t (lst1, t2)

let rec msort' lst =
  match lst with
  | [] | [_] -> lst
  | _ ->
      let lst1, lst2 = split_t lst in
      merge_t (msort' lst1, msort' lst2)

let bigl3 = []

let rec msort_g ord lst =
  let rec split_g lst =
    let rec split_helper acc n lst =
      if n = 0 then (List.rev acc, lst)
      else
        match lst with
        | [] -> (List.rev acc, [])
        | h :: t -> split_helper (h :: acc) (n - 1) t
    in
    split_helper [] (List.length lst / 2) lst
  in
  let rec merge_g (lst1, lst2) =
    match (lst1, lst2) with
    | [], lst | lst, [] -> lst
    | h1 :: t1, h2 :: t2 -> if ord h1 h2 then h1 :: merge_g (t1, lst2) else h2 :: merge_g (lst1, t2)
  in
  match lst with
  | [] | [_] -> lst
  | _ ->
      let lst1, lst2 = split_g lst in
      merge_g (msort_g ord lst1, msort_g ord lst2)
