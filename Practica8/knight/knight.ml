(* knight.ml *)

exception Not_found

let rec is_valid_move m n obstacles visited (x, y) =
  x >= 0 && x < m && y >= 0 && y < n && not (List.mem (x, y) obstacles) && not (List.mem (x, y) visited)

let rec tour_helper m n obstacles visited current end_pos path =
  if current = end_pos then
    List.rev (current :: path)
  else
    let possible_moves =
      [ (current |> fst - 2, current |> snd - 1);
        (current |> fst - 2, current |> snd + 1);
        (current |> fst - 1, current |> snd - 2);
        (current |> fst - 1, current |> snd + 2);
        (current |> fst + 1, current |> snd - 2);
        (current |> fst + 1, current |> snd + 2);
        (current |> fst + 2, current |> snd - 1);
        (current |> fst + 2, current |> snd + 1) ]
    in
    let valid_moves =
      List.filter (is_valid_move m n obstacles (current :: visited)) possible_moves
    in
    match valid_moves with
    | [] -> raise Not_found
    | hd :: tl -> tour_helper m n obstacles (current :: visited) hd (current :: path)

let tour m n obstacles ini fin =
  try
    tour_helper m n obstacles [] ini [] fin
  with Not_found -> raise Not_found

let min_tour m n obstacles ini fin =
  let rec min_tour_helper acc current_path =
    let current_length = List.length current_path in
    let min_length = List.length acc in
    if current_length < min_length then current_path
    else if current_length = min_length && List.length current_path < min_length then current_path
    else
      let last_position = List.hd (List.rev current_path) in
      let possible_moves =
        [ (last_position |> fst - 2, last_position |> snd - 1);
          (last_position |> fst - 2, last_position |> snd + 1);
          (last_position |> fst - 1, last_position |> snd - 2);
          (last_position |> fst - 1, last_position |> snd + 2);
          (last_position |> fst + 1, last_position |> snd - 2);
          (last_position |> fst + 1, last_position |> snd + 2);
          (last_position |> fst + 2, last_position |> snd - 1);
          (last_position |> fst + 2, last_position |> snd + 1) ]
      in
      let valid_moves =
        List.filter (is_valid_move m n obstacles current_path) possible_moves
      in
      match valid_moves with
      | [] -> raise Not_found
      | hd :: tl ->
          let new_path = current_path @ [hd] in
          min_tour_helper acc (tour m n obstacles ini fin hd new_path)
  in
  try
    min_tour_helper [] [ini]
  with Not_found -> raise Not_found

let min_tour4D m n obstacles ini fin =
  let rec min_tour4D_helper acc current_path =
    let current_length = List.length current_path in
    let min_length = List.length acc in
    if current_length < min_length then current_path
    else if current_length = min_length && List.length current_path < min_length then current_path
    else
      let last_position = List.hd (List.rev current_path) in
      let possible_moves =
        [ (last_position |> fst - 2, last_position |> snd - 1);
          (last_position |> fst - 2, last_position |> snd + 1);
          (last_position |> fst - 1, last_position |> snd - 2);
          (last_position |> fst - 1, last_position |> snd + 2);
          (last_position |> fst + 1, last_position |> snd - 2);
          (last_position |> fst + 1, last_position |> snd + 2);
          (last_position |> fst + 2, last_position |> snd - 1);
          (last_position |> fst + 2, last_position |> snd + 1) ]
      in
      let valid_moves =
        List.filter (is_valid_move m n obstacles current_path) possible_moves
      in
      match valid_moves with
      | [] -> raise Not_found
      | hd :: tl ->
          let new_path = current_path @ [hd] in
          min_tour4D_helper acc (tour m n obstacles ini fin hd new_path)
  in
  try
    min_tour4D_helper [] [ini]
  with Not_found -> raise Not_found
