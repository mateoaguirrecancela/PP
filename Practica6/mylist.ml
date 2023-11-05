let hd = function
  | [] -> failwith "hd: empty list"
  | h :: _ -> h

let tl = function
  | [] -> failwith "tl: empty list"
  | _ :: t -> t

let rec length lst =
  match lst with
  | [] -> 0
  | _ :: t -> 1 + length t

let rec compare_lengths l1 l2 =
  match (l1, l2) with
  | ([], []) -> 0
  | ([], _) -> -1
  | (_, []) -> 1
  | _ :: t1, _ :: t2 -> compare_lengths t1 t2

let rec compare_length_with lst n =
  match (lst, n) with
  | ([], 0) -> 0
  | ([], _) -> -1
  | (_, 0) -> 1
  | _ :: t, n -> compare_length_with t (n - 1)

let rec init n f =
  if n < 0 then
    failwith "init: negative size"
  else if n = 0 then
    []
  else
    f (n - 1) :: init (n - 1) f

let rec nth lst n =
  match (lst, n) with
  | ([], _) -> failwith "nth: index out of bounds"
  | (_, 0) -> hd lst
  | _ :: t, n -> nth t (n - 1)

let rec append l1 l2 =
  match l1 with
  | [] -> l2
  | h :: t -> h :: append t l2

let rec rev_append l1 l2 =
  match l1 with
  | [] -> l2
  | h :: t -> rev_append t (h :: l2)

let rev lst =
  rev_append lst []

let rec concat lst =
  match lst with
  | [] -> []
  | h :: t -> append h (concat t)

let rec flatten = concat

let rec split lst =
  let rec split_helper left right = function
    | [] -> (rev left, rev right)
    | (a, b) :: t -> split_helper (a :: left) (b :: right) t
  in
  split_helper [] [] lst

let rec combine l1 l2 =
  let rec combine_helper acc lst1 lst2 =
    match (lst1, lst2) with
    | ([], []) -> rev acc
    | (h1 :: t1, h2 :: t2) -> combine_helper ((h1, h2) :: acc) t1 t2
    | _ -> failwith "combine: lists have different lengths"
  in
  combine_helper [] l1 l2

let rec map f lst =
  match lst with
  | [] -> []
  | h :: t -> f h :: map f t

let rec map2 f l1 l2 =
  match (l1, l2) with
  | ([], []) -> []
  | (h1 :: t1, h2 :: t2) -> f h1 h2 :: map2 f t1 t2
  | _ -> failwith "map2: lists have different lengths"

let rec rev_map f lst =
  let rec rev_map_helper acc = function
    | [] -> rev acc
    | h :: t -> rev_map_helper (f h :: acc) t
  in
  rev_map_helper [] lst

let rec for_all p lst =
  match lst with
  | [] -> true
  | h :: t -> p h && for_all p t

let rec exists p lst =
  match lst with
  | [] -> false
  | h :: t -> p h || exists p t

let rec mem x lst =
  match lst with
  | [] -> false
  | h :: t -> x = h || mem x t

let rec find p lst =
  match lst with
  | [] -> failwith "find: element not found"
  | h :: t -> if p h then h else find p t

let rec filter p lst =
  match lst with
  | [] -> []
  | h :: t -> if p h then h :: filter p t else filter p t

let rec find_all p lst = filter p lst

let rec partition p lst =
  let rec partition_helper left right = function
    | [] -> (rev left, rev right)
    | h :: t ->
      if p h then
        partition_helper (h :: left) right t
      else
        partition_helper left (h :: right) t
  in
  partition_helper [] [] lst

let rec fold_left f acc lst =
  match lst with
  | [] -> acc
  | h :: t -> fold_left f (f acc h) t

let rec fold_right f lst acc =
  match lst with
  | [] -> acc
  | h :: t -> f h (fold_right f t acc)

let rec assoc x lst =
  match lst with
  | [] -> failwith "assoc: key not found"
  | (k, v) :: t -> if k = x then v else assoc x t

let rec mem_assoc x lst =
  match lst with
  | [] -> false
  | (k, _) :: t -> k = x || mem_assoc x t

let rec remove_assoc x lst =
  match lst with
  | [] -> []
  | (k, _) :: t -> if k = x then t else (k, assoc x t) :: remove_assoc x t