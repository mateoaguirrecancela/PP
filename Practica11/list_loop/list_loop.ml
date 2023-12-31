let length lst =
  let count = ref 0 in
  let i = ref 0 in
  let () =
    while !i < List.length lst do
      incr count;
      incr i
    done
  in
  !count

let last lst =
  let result = ref None in
  let () =
    List.iter (fun x -> result := Some x) lst
  in
  match !result with
  | Some x -> x
  | None -> failwith "last"

let nth lst n =
  let result = ref None in
  let () =
    List.iteri (fun i x -> if i = n then result := Some x) lst
  in
  match !result with
  | Some x -> x
  | None -> failwith "nth"

let rev lst =
  let reversed = ref [] in
  let () =
    List.iter (fun x -> reversed := x :: !reversed) lst
  in
  !reversed

let append lst1 lst2 =
  let result = ref [] in
  let i = ref 0 in
  let () =
    while !i < List.length lst1 do
      result := List.nth lst1 !i :: !result;
      incr i
    done;
    i := 0;
    while !i < List.length lst2 do
      result := List.nth lst2 !i :: !result;
      incr i
    done
  in
  !result

let concat lst_lst =
  let result = ref [] in
  let i = ref 0 in
  let j = ref 0 in
  let () =
    while !i < List.length lst_lst do
      let inner_list = List.nth lst_lst !i in
      j := 0;
      while !j < List.length inner_list do
        result := (List.nth inner_list !j) :: !result;
        incr j
      done;
      incr i
    done
  in
  !result

let for_all pred lst =
  let result = ref true in
  let () =
    List.iter (fun x -> if not (pred x) then result := false) lst
  in
  !result

let exists pred lst =
  let result = ref false in
  let () =
    List.iter (fun x -> if pred x then result := true) lst
  in
  !result

let find_opt pred lst =
  let result = ref None in
  let () =
    List.iter (fun x -> if pred x then result := Some x) lst
  in
  !result

let iter f lst =
  let () =
    List.iter (fun x -> f x) lst
  in
  ()

let fold_left f acc lst =
  let result = ref acc in
  let () =
    List.iter (fun x -> result := f !result x) lst
  in
  !result
