let rec threatens (x, y) = function
| [] -> false
| (a, b) :: rest ->
  y = b || abs (x - a) = abs (y - b) || threatens (x, y) rest

let rec place_queens n row queens_cols =
if row = n then [queens_cols]
else
  List.flatten
    (List.map
       (fun col ->
         if not (threatens (row + 1, col) queens_cols) then
           place_queens n (row + 1) ((row + 1, col) :: queens_cols)
         else [])
       (List.init n (fun i -> i + 1)))

let queens n =
if n < 0 then
  invalid_arg "queens: the dimension should be a non-negative integer"
else place_queens n 0 []

let is_queens_sol n sol =
  let rec no_conflict x y sol =
    match sol with
    | [] -> true
    | (a, b) :: t ->
      y <> b && x - y <> a - b && x + y <> a + b && no_conflict x y t
  in

  let rec valid_positions sol =
    match sol with
    | [] -> true
    | (x, y) :: t ->
      x > 0 && x <= n && y > 0 && y <= n && no_conflict x y t && valid_positions t
  in

  List.length sol = n && valid_positions sol