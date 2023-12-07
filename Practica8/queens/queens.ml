(* queens.ml *)

let rec conflict (r, c) queens =
  List.exists (fun (r', c') -> r = r' || c = c' || abs (r - r') = abs (c - c')) queens

let rec place_queen n row queens =
  if row > n then [queens]
  else
    let try_place col =
      if not (conflict (row, col) queens) then
        place_queen n (row + 1) ((row, col) :: queens)
      else []
    in
    List.flatten (List.map try_place (List.init n succ))

let queens n =
  if n < 0 then invalid_arg "queens: negative dimension";
  place_queen n 1 []

let is_queens_sol n sol =
  let rec valid_solution queens =
    match queens with
    | [] -> true
    | (r, c) :: rest ->
        not (conflict (r, c) rest) && valid_solution rest
  in
  List.length sol = n && valid_solution sol
