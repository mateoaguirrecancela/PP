(*Definición de tipos y firma de módulo*)
module type ORD = sig
  type t
  val compare : t -> t -> int
end

module type SORT = sig
  type t
  val insert_t : t -> t list -> t list
  val isort_t : t list -> t list
  val crono : ('a -> 'b) -> 'a -> float
  val isort_g : (t -> t -> bool) -> t list -> t list
  val split_t : t list -> t list * t list
  val merge_t : t list * t list -> t list
  val msort' : t list -> t list
  val msort_g : (t -> t -> bool) -> t list -> t list
end

(*Implementación del módulo*)
module MakeSort (Ord : ORD) : SORT with type t = Ord.t = struct
  type t = Ord.t

  (*Funciones de ordenación por inserción*)
  let rec insert_t x lst =
    match lst with
    | [] -> [x]
    | h :: t -> if Ord.compare x h <= 0 then x :: h :: t else h :: insert_t x t

  let rec isort_t lst =
    match lst with
    | [] -> []
    | h :: t -> insert_t h (isort_t t)

  (*Función crono para medir tiempos de ejecución*)
  let crono f x =
    let t = Sys.time () in
    let _ = f x in
    Sys.time () -. t

  (*Función de ordenación por inserción con relación de orden*)
  let rec isort_g compare_fn lst =
    let rec insert_g x lst =
      match lst with
      | [] -> [x]
      | h :: t -> if compare_fn x h then x :: h :: t else h :: insert_g x t
    in
    match lst with
    | [] -> []
    | h :: t -> insert_g h (isort_g compare_fn t)

  (*Funciones de ordenación por fusión*)
  let rec split_t lst =
    match lst with
    | h1 :: h2 :: t -> let t1, t2 = split_t t in (h1 :: t1, h2 :: t2)
    | _ -> (lst, [])

  let rec merge_t (l1, l2) =
    match (l1, l2) with
    | [], l | l, [] -> l
    | h1 :: t1, h2 :: t2 -> if Ord.compare h1 h2 <= 0 then h1 :: merge_t (t1, l2) else h2 :: merge_t (l1, t2)

  let rec msort' lst =
    match lst with
    | [] | [_] -> lst
    | _ ->
        let l1, l2 = split_t lst in
        merge_t (msort' l1, msort' l2)

  (*Función de ordenación por fusión con relación de orden*)
  let msort_g compare_fn lst =
    let rec split_g lst =
      match lst with
      | h1 :: h2 :: t -> let t1, t2 = split_g t in (h1 :: t1, h2 :: t2)
      | _ -> (lst, [])
    in
    let rec merge_g (l1, l2) =
      match (l1, l2) with
      | [], l | l, [] -> l
      | h1 :: t1, h2 :: t2 -> if compare_fn h1 h2 then h1 :: merge_g (t1, l2) else h2 :: merge_g (l1, t2)
    in
    match lst with
    | [] | [_] -> lst
    | _ ->
        let l1, l2 = split_g lst in
        merge_g (msort_g compare_fn l1, msort_g compare_fn l2)
end



(*************************************************************************************************)



(* Definición de bigl *)
let bigl = [10000; 9999; 9998; 9997; 9996; 9995; ...] (* Lista lo suficientemente grande para causar desbordamiento de pila *)

(* Funciones insert_t e isort_t *)
let rec insert_t x lst acc =
  match lst with
  | [] -> List.rev (x :: acc)
  | h :: t -> if x <= h then List.rev_append acc (x :: h :: t) else insert_t x t (h :: acc)

let rec isort_t lst acc =
  match lst with
  | [] -> List.rev acc
  | h :: t -> isort_t t (insert_t h acc [])

(* Función rlist *)
let rlist n =
  let rec generate_random_list n acc =
    if n = 0 then acc
    else generate_random_list (n - 1) (Random.int 10000 :: acc)
  in
  generate_random_list n []

(* Función crono *)
let crono f x =
  let t = Sys.time () in
  let _ = f x in
  Sys.time () -. t

(* Listas *)
let lc1 = [1; 2; 3; ...] (* Lista creciente de 10,000 elementos *)
let lc2 = [1; 2; 3; ...] (* Lista creciente de 20,000 elementos *)
let ld1 = [10000; 9999; ...] (* Lista decreciente de 10,000 elementos *)
let ld2 = [20000; 19999; ...] (* Lista decreciente de 20,000 elementos *)
let lr1 = rlist 10000 (* Lista aleatoria de 10,000 elementos *)
let lr2 = rlist 20000 (* Lista aleatoria de 20,000 elementos *)

(* Función isort_g *)
let isort_g cmp lst =
  let rec insert_g x lst acc =
    match lst with
    | [] -> List.rev (x :: acc)
    | h :: t -> if cmp x h then List.rev_append acc (x :: h :: t) else insert_g x t (h :: acc)
  in
  let rec isort_g_aux lst acc =
    match lst with
    | [] -> List.rev acc
    | h :: t -> isort_g_aux t (insert_g h acc [])
  in
  isort_g_aux lst []

(* Definición de bigl2 *)
let bigl2 = [10000; 9999; 9998; 9997; 9996; 9995; ...] (* Lista lo suficientemente grande para causar desbordamiento de pila en msort *)

(* Funciones split_t y merge_t *)
let split_t lst =
  let rec split_t_aux lst acc1 acc2 =
    match lst with
    | [] -> (List.rev acc1, List.rev acc2)
    | h1 :: t1 -> (
        match t1 with
        | h2 :: t2 -> split_t_aux t2 (h1 :: acc1) (h2 :: acc2)
        | _ -> (List.rev (h1 :: acc1), List.rev acc2)
      )
  in
  split_t_aux lst [] []

let rec merge_t (lst1, lst2) =
  match (lst1, lst2) with
  | [], lst | lst, [] -> lst
  | h1 :: t1, h2 :: t2 -> if h1 <= h2 then h1 :: merge_t (t1, lst2) else h2 :: merge_t (lst1, t2)

(* Función msort' *)
let rec msort' lst =
  match lst with
  | [] | [_] -> lst
  | _ ->
      let lst1, lst2 = split_t lst in
      merge_t (msort' lst1, msort' lst2)

(* Definición de bigl3 *)
let bigl3 = [] (* Lista vacía *)

(* Función msort_g *)
let msort_g cmp lst =
  let rec split_g lst =
    match lst with
    | h1 :: h2 :: t -> let t1, t2 = split_g t in (h1 :: t1, h2 :: t2)
    | _ -> (lst, [])
  in
  let rec merge_g (lst1, lst2) =
    match (lst1, lst2) with
    | [], lst | lst, [] -> lst
    | h1 :: t1, h2 :: t2 -> if cmp h1 h2 then h1 :: merge_g (t1, lst2) else h2 :: merge_g (lst1, t2)
  in
  let rec msort_g_aux lst =
    match lst with
    | [] | [_] -> lst
    | _ ->
        let lst1, lst2 = split_g lst in
        merge_g (msort_g_aux lst1, msort_g_aux lst2)
  in
  msort_g_aux lst
