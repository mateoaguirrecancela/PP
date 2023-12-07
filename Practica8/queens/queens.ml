let rec not_attacked x queen_positions =
  List.for_all
    (fun (y, _) ->
      x <> y && abs (x - y) <> abs (List.length queen_positions + 1 - snd _))
    queen_positions

let rec add_queen n queen_positions acc =
  if n = 0 then [queen_positions] @ acc
  else
    let valid_positions =
      List.filter
        (fun x -> not_attacked x queen_positions)
        (List.init n (fun x -> x + 1))
    in
    List.fold_left
      (fun acc' pos ->
        add_queen
          (n - 1)
          ((n, pos) :: queen_positions)
          (List.rev_append acc' acc))
      [] valid_positions

let queens n =
  if n < 0 then invalid_arg "queens"
  else add_queen n [] []

let is_queens_sol n sol =
  if n < 0 || List.length sol <> n then invalid_arg "is_queens_sol"
  else
    let rec not_attacked' x queen_positions =
      List.for_all
        (fun (y, _) -> x <> y && abs (x - y) <> abs (List.length queen_positions + 1 - snd _))
        queen_positions
    in
    List.for_all
      (fun (x, y) -> not_attacked' x (List.rev_append (List.tl sol) [(x, y)]))
      sol
