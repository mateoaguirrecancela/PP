(*generar los movimientos posibles de un caballo*)
let possible_moves (x, y) =
  [
    (x + 1, y + 2); (x + 2, y + 1);
    (x + 2, y - 1); (x + 1, y - 2);
    (x - 1, y - 2); (x - 2, y - 1);
    (x - 2, y + 1); (x - 1, y + 2);
  ]

(*verificar si una casilla esta dentro del tablero*)
let inside_board m n (x, y) = x >= 0 && x < m && y >= 0 && y < n

(*verificar si una casilla esta bloqueada*)
let is_blocked obstacles (x, y) = List.mem (x, y) obstacles

(*encontrar un camino de caballo*)
let rec find_tour m n obstacles current_path current_pos end_pos =
  if current_pos = end_pos then
    List.rev ((current_pos :: current_path))
  else
    let next_moves =
      List.filter
        (fun move ->
          inside_board m n move
          && not (is_blocked obstacles move)
          && not (List.mem move current_path))
        (possible_moves current_pos)
    in
    match next_moves with
    | [] -> raise Not_found
    | _ ->
      List.fold_left
        (fun acc move ->
          try find_tour m n obstacles (current_pos :: current_path) move end_pos with
          | Not_found -> acc)
        [] next_moves


let tour m n obstacles ini fin =
  find_tour m n obstacles [] ini fin

(*calcular la longitud de un camino*)
let path_length path = List.length path

(*encontrar caminos minimos*)
let rec find_min_tour m n obstacles current_path current_pos end_pos min_path_length min_path =
  if current_pos = end_pos then
    let current_length = path_length current_path in
    if current_length < min_path_length then
      (current_length, List.rev (current_pos :: current_path))
    else
      (min_path_length, min_path)
  else
    let next_moves =
      List.filter
        (fun move ->
          inside_board m n move
          && not (is_blocked obstacles move)
          && not (List.mem move current_path))
        (possible_moves current_pos)
    in
    match next_moves with
    | [] -> raise Not_found
    | _ ->
      List.fold_left
        (fun (acc_length, acc_path) move ->
          try
            let new_path = find_min_tour m n obstacles (current_pos :: current_path) move end_pos acc_length acc_path in
            if fst new_path < acc_length then
              new_path
            else
              (acc_length, acc_path)
          with
          | Not_found -> (acc_length, acc_path))
        (min_path_length, min_path) next_moves


let min_tour m n obstacles ini fin =
  let (_, path) = find_min_tour m n obstacles [] ini fin max_int [] in
  path

(*4D*)
let possible_moves_4D (x, y) =
  [
    (x + 1, y + 2); (x + 2, y + 1);
    (x + 2, y - 1); (x + 1, y - 2);
    (x - 1, y - 2); (x - 2, y - 1);
    (x - 2, y + 1); (x - 1, y + 2);
    (*movimientos adicionales para la 4D*)
    (x + 1, y + 2); (x + 2, y + 1);
    (x + 2, y - 1); (x + 1, y - 2);
    (x - 1, y - 2); (x - 2, y - 1);
    (x - 2, y + 1); (x - 1, y + 2);
  ]
  
let inside_board_4D m n (x, y) = x >= 0 && x < m && y >= 0 && y < n
  
let is_blocked_4D obstacles (x, y) = List.mem (x, y) obstacles
  
let rec find_tour_4D m n obstacles current_path current_pos end_pos =
  if current_pos = end_pos then
    List.rev ((current_pos :: current_path))
  else
    let next_moves =
      List.filter
        (fun move ->
          inside_board_4D m n move
          && not (is_blocked_4D obstacles move)
          && not (List.mem move current_path))
        (possible_moves_4D current_pos)
    in
    match next_moves with
    | [] -> raise Not_found
    | _ ->
      List.fold_left
        (fun acc move ->
          try find_tour_4D m n obstacles (current_pos :: current_path) move end_pos with
          | Not_found -> acc)
        [] next_moves
 
let min_tour4D m n obstacles ini fin =
  let (_, path) = find_min_tour m n obstacles [] ini fin max_int [] in
  path