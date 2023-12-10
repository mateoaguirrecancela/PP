(*odenacion por insercion*)
let rec insert x = function
    [] -> [x]
  | h::t -> if x <= h then x :: h :: t
            else h :: insert x t

let rec isort = function
    [] -> []
  | h::t -> insert h (isort t)

(*lista aleatoria*)
let rec rlist_with_range n min max =
  if n < 0 then []
  else (Random.int (max - min + 1) + min) :: rlist_with_range (n - 1) min max

let rlist n =
  let min_value = 0 in
  let max_value = 10000 in
  rlist_with_range n min_value max_value


(*lista muy grande*)
let bigl = List.init 200000 (fun x -> (10000 - x))

(*implementaciones recursivas terminales*)
let insert_t x lst =
  let rec insert_acc acc x = function
    | [] -> List.rev (x :: acc)
    | h :: t ->
      if x <= h then insert_acc (x :: acc) h t
      else List.rev_append (h :: acc) (x :: t)
  in
  insert_acc [] x lst

let isort_t lst =
  let rec isort_acc acc = function
    | [] -> List.rev acc
    | h :: t -> isort_acc (insert_t h acc) t
  in
  isort_acc [] lst
  
(*tiempo de ejecucion*)
let crono f x =
  let t = Sys.time () in
  let _ = f x in
  Sys.time () -. t

(*listas*)
let lc1 = List.init 10000 (fun x -> x)
let lc2 = List.init 20000 (fun x -> x)

let ld1 = List.init 10000 (fun x -> (10000 - x))
let ld2 = List.init 20000 (fun x -> (20000 - x))

let lr1 = rlist 10000
let lr2 = rlist 20000

(*comprobar si ambas funciones dan el mismo resultado*)
let check_isort_lists () =

  let result_isort_lc1 = isort lc1 in
  let result_isort_t_lc1 = isort_t lc1 in
  let result_isort_lc2 = isort lc2 in
  let result_isort_t_lc2 = isort_t lc2 in

  let result_isort_ld1 = isort ld1 in
  let result_isort_t_ld1 = isort_t ld1 in
  let result_isort_ld2 = isort ld2 in
  let result_isort_t_ld2 = isort_t ld2 in

  let result_isort_lr1 = isort lr1 in
  let result_isort_t_lr1 = isort_t lr1 in
  let result_isort_lr2 = isort lr2 in
  let result_isort_t_lr2 = isort_t lr2 in

  let equal_lists l1 l2 =
    List.sort compare l1 = List.sort compare l2
  in

  assert (equal_lists result_isort_lc1 result_isort_t_lc1);
  assert (equal_lists result_isort_lc2 result_isort_t_lc2);

  assert (equal_lists result_isort_ld1 result_isort_t_ld1);
  assert (equal_lists result_isort_ld2 result_isort_t_ld2);

  assert (equal_lists result_isort_lr1 result_isort_t_lr1);
  assert (equal_lists result_isort_lr2 result_isort_t_lr2);

  Printf.printf "isort_t produce el mismo resultado que isort en todas las listas\n"

(*medicion del tiempo de ejecucion*)
let tc1 = crono isort lc1
let ttc1 = crono isort_t lc1

let tc2 = crono isort lc2
let ttc2 = crono isort_t lc2

let td1 = crono isort ld1
let ttd1 = crono isort_t ld1

let td2 = crono isort ld2
let ttd2 = crono isort_t ld2

let tr1 = crono isort lr1
let ttr1 = crono isort_t lr1

let tr2 = crono isort lr2
let ttr2 = crono isort_t lr2

(*
Printf.printf "Tiempo de ejecución de isort en lc1: %f segundos\n" tc1
Printf.printf "Tiempo de ejecución de isort_t en lc1: %f segundos\n" ttc1
Printf.printf "Tiempo de ejecución de isort en lc2: %f segundos\n" tc2
Printf.printf "Tiempo de ejecución de isort_t en lc2: %f segundos\n" ttc2
Printf.printf "Tiempo de ejecución de isort en ld1: %f segundos\n" td1
Printf.printf "Tiempo de ejecución de isort_t en ld1: %f segundos\n" ttd1
Printf.printf "Tiempo de ejecución de isort en ld2: %f segundos\n" td2
Printf.printf "Tiempo de ejecución de isort_t en ld2: %f segundos\n" ttd2
Printf.printf "Tiempo de ejecución de isort en lr1: %f segundos\n" tr1
Printf.printf "Tiempo de ejecución de isort_t en lr1: %f segundos\n" ttr1
Printf.printf "Tiempo de ejecución de isort en lr2: %f segundos\n" tr2
Printf.printf "Tiempo de ejecución de isort_t en lr2: %f segundos\n" ttr2
*)

(*
Observando los tiempos de ejecucion, isort_t es mucho eficiente en todos los casos,
excepto en las listas descendentes que tarda mas que isort. La gran diferencia de tiempos
entre estas dos implementaciones es debida a que la version recursiva terminal evita
el desbordamiento del stack de llamadas. Sin embargo, en la lista desordenada isort_t
produce tiempos mas altos debido a la naturaleza de la funcion, en esta situacion las
inserciones se deben hacer al final de la lista y es aqui donde mas sufre esta funcion
*)

(*implementacion recursiva terminal que tome como argumento la relacion de orden que se desea emplear para la ordenacion*)
let rec insert_g cmp x lst =
  let rec insert_acc acc x = function
    | [] -> List.rev (x :: acc)
    | h :: t ->
      if cmp x h then insert_acc (x :: acc) h t
      else List.rev_append (h :: acc) (x :: t)
  in
  insert_acc [] x lst

let isort_g cmp lst =
  let rec isort_acc acc = function
    | [] -> List.rev acc
    | h :: t -> isort_acc (insert_g cmp h acc) t
  in
  isort_acc [] lst


(*ordenacion por fusion*)
let rec split l = match l with
    h1::h2::t -> let t1, t2 = split t
                 in h1::t1, h2::t2
  | _ -> l, []

let rec merge (l1,l2) = match l1, l2 with
    [], l | l, [] -> l
  | h1::t1, h2::t2 -> if h1 <= h2 then h1 :: merge (t1, l2)
                      else h2 :: merge (l1, t2)

let rec msort l = match l with
    [] | [_] -> l
  | _ -> let l1, l2 = split l
         in merge (msort l1, msort l2)

(*comprobar si msort produce el mismo resultado que isort en las listas dadas*)
let check_msort_isort () =
  let result_msort_lc1 = msort lc1 in
  let result_isort_lc1 = isort lc1 in
  let result_msort_lc2 = msort lc2 in
  let result_isort_lc2 = isort lc2 in

  let result_msort_ld1 = msort ld1 in
  let result_isort_ld1 = isort ld1 in
  let result_msort_ld2 = msort ld2 in
  let result_isort_ld2 = isort ld2 in

  let result_msort_lr1 = msort lr1 in
  let result_isort_lr1 = isort lr1 in
  let result_msort_lr2 = msort lr2 in
  let result_isort_lr2 = isort lr2 in

  let equal_lists l1 l2 =
    List.sort compare l1 = List.sort compare l2
  in

  assert (equal_lists result_msort_lc1 result_isort_lc1);
  assert (equal_lists result_msort_lc2 result_isort_lc2);

  assert (equal_lists result_msort_ld1 result_isort_ld1);
  assert (equal_lists result_msort_ld2 result_isort_ld2);

  assert (equal_lists result_msort_lr2 result_isort_lr2);
  assert (equal_lists result_msort_lr1 result_isort_lr1);

  Printf.printf "msort produce el mismo resultado que isort en todas las listas\n"

(*lista muy grande*)
let bigl2 = List.init 200000 (fun x -> (10000 - x))

(*implementaciones recursivas terminales*)
let rec split_t lst =
  let rec split_acc acc1 acc2 = function
    | [] -> (List.rev acc1, List.rev acc2)
    | h1::h2::t -> split_acc (h1 :: acc1) (h2 :: acc2) t
    | h::t -> split_acc (h :: acc1) acc2 t
  in
  split_acc [] [] lst

let rec merge_t (l1, l2) =
  let rec merge_acc acc l1 l2 = match l1, l2 with
    | [], l | l, [] -> List.rev_append acc l
    | h1::t1, h2::t2 ->
      if h1 <= h2 then merge_acc (h1 :: acc) t1 l2
      else merge_acc (h2 :: acc) l1 t2
  in
  merge_acc [] l1 l2

let rec msort' l =
  match l with
  | [] | [_] -> l
  | _ ->
    let (l1, l2) = split_t l in
    merge_t (msort' l1, msort' l2)

(*lista muy grande*)
let bigl3 = []

(*
Aunque la función msort' en si misma no es recursiva terminal, las llamadas recursivas a msort' ocurren en dos sublistas más pequeñas despues de aplicar split_t.
En cada llamada recursiva, se reducen las sublistas y eventualmente llegaran a listas de tamaño 0 o 1, lo que detendra la recursion y evitara un "stack overflow".
En conclusion, no se produce un desbordamiento de pila en este caso es porque, a medida que se realiza la recursion, el tamaño de las sublistas se reduce de manera
significativa en cada nivel de la recursion. Esto significa que la profundidad maxima de la pila no crece en funcion del tamaño total de la lista original.
*)

(*medicion de tiempos de ejecucion*)
let t_msort'_lc1 = crono msort' lc1
let t_msort'_lc2 = crono msort' lc2
let t_msort'_ld1 = crono msort' ld1
let t_msort'_ld2 = crono msort' ld2
let t_msort'_lr1 = crono msort' lr1
let t_msort'_lr2 = crono msort' lr2

(*
Printf.printf "Tiempo de ejecución de msort' en lc1: %f segundos\n" t_msort'_lc1;
Printf.printf "Tiempo de ejecución de msort' en lc2: %f segundos\n" t_msort'_lc2;
Printf.printf "Tiempo de ejecución de msort' en ld1: %f segundos\n" t_msort'_ld1;
Printf.printf "Tiempo de ejecución de msort' en ld2: %f segundos\n" t_msort'_ld2;
Printf.printf "Tiempo de ejecución de msort' en lr1: %f segundos\n" t_msort'_lr1;
Printf.printf "Tiempo de ejecución de msort' en lr2: %f segundos\n" t_msort'_lr2;
*)

(*
Los tiempos de ejecucion de msort' son muy buenos listas ordenadas, invertidas y aleatorias. Esto se debe a la gran eficiencia que tiene la naturaleza del algortimo,
la implementación aprovecha el algoritmo divide y venceras para lograr tiempos de ejecución eficientes. split_t genera sublistas que contienen muchos menos elementos
que a merge_t le reduce el esfuerzo necesario para poder combinarlas
*)

(*comparacion de los tiempos de ejecucion*)
let t_msort_lr1 = crono msort lr1
let t_msort_lr2 = crono msort lr2

(*
Printf.printf "Tiempo de ejecución de msort en lr1: %f segundos\n" t_msort_lr1;
Printf.printf "Tiempo de ejecución de msort' en lr1: %f segundos\n" t_msort'_lr1;
Printf.printf "Tiempo de ejecución de msort en lr2: %f segundos\n" t_msort_lr2;
Printf.printf "Tiempo de ejecución de msort' en lr2: %f segundos\n" t_msort'_lr2;
*)

(*
En este caso es mas eficiente la version no rescursiva terminal que la recursiva terminal. Esto se debe a que msort puede ser mas eficiente en algunos casos, como este,
ya que evita el costo de creacion de marcos de pila adicionales para cada llamada recursiva. En cambio, lo que hace es utilizar un enfoque iterativo que puede es mas
eficiente en terminos de consumo de memoria y tiempo de ejecucion para estos casos
*)

(*implementacion que tome como argumento la relacion de orden a emplear*)
let rec split_g lst =
  let rec split_acc acc1 acc2 = function
    | [] -> (List.rev acc1, List.rev acc2)
    | h1::h2::t -> split_acc (h1 :: acc1) (h2 :: acc2) t
    | h::t -> split_acc (h :: acc1) acc2 t
  in
  split_acc [] [] lst

let rec merge_g cmp (l1, l2) =
  let rec merge_acc acc l1 l2 = match l1, l2 with
    | [], l | l, [] -> List.rev_append acc l
    | h1::t1, h2::t2 ->
      if cmp h1 h2 then merge_acc (h1 :: acc) t1 l2
      else merge_acc (h2 :: acc) l1 t2
  in
  merge_acc [] l1 l2

let rec msort_g cmp l =
  match l with
  | [] | [_] -> l
  | _ ->
    let (l1, l2) = split_g l in
    merge_g cmp (msort_g cmp l1, msort_g cmp l2)