let rec safe (row, col) queens =
  let is_safe (r, c) = c <> col && abs (r - row) <> abs (c - col) in
  List.for_all is_safe queens

let rec place_queens n row queens =
  if row > n then [queens]
  else
    List.flatten
      (List.map
         (fun col ->
           if safe (row, col) queens then
             place_queens n (row + 1) ((row, col) :: queens)
           else [])
         (List.init n (fun i -> i + 1)))

let queens n =
  if n < 0 then
    invalid_arg "queens: negative dimension"
  else if n = 0 then
    [[]]
  else
    place_queens n 1 []

let is_queens_sol n sol =
  let rec no_conflict r c queens =
    match queens with
    | [] -> true
    | (r', c') :: rest ->
        c <> c' && (r - r' <> c - c') && (r' - r <> c - c') && no_conflict r c rest
  in
  List.length sol = n && List.for_all (fun (r, c) -> no_conflict r c sol) sol
