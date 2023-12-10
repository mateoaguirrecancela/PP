(* queens.ml *)
let rec is_safe row col queens =
  let is_safe_cell (r, c) = row <> r && col <> c && abs (row - r) <> abs (col - c) in
  List.for_all is_safe_cell queens

let rec place_queen n row queens =
  if row > n then [queens]
  else
    let try_place col =
      if is_safe row col queens then
        let new_queen = (row, col) in
        let new_queens = new_queen :: queens in
        place_queen n (row + 1) new_queens
      else []
    in
    List.flatten (List.map try_place (List.init n ((+) 1)))

let queens n = place_queen n 1 []

let is_queens_sol n sol =
  let rec is_valid row col queens =
    match queens with
    | [] -> true
    | (r, c) :: rest ->
      row <> r && col <> c && abs (row - r) <> abs (col - c) && is_valid row col rest
  in
  List.length sol = n && List.for_all (fun (row, col) -> is_valid row col sol) sol























































  (* queens.ml *)

(* Función auxiliar para comprobar si una reina amenaza a otra *)
let rec threatens (x, y) = function
| [] -> false
| (a, b) :: rest ->
  y = b || abs (x - a) = abs (y - b) || threatens (x, y) rest

(* Función auxiliar para generar las posibles soluciones para n reinas *)
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

(* Función principal queens *)
let queens n =
if n < 0 then
  invalid_arg "queens: the dimension should be a non-negative integer"
else place_queens n 0 []

(* Función auxiliar para comprobar si una lista es una solución válida *)
let is_queens_sol n sol =
  let rec threatens_queen queen rest =
    match rest with
    | [] -> false
    | (a, b) :: xs -> b = snd queen || abs (fst queen - a) = abs (snd queen - b) || threatens_queen queen xs
  in
  List.length sol = n && not (List.exists (fun queen -> threatens_queen queen sol) sol)
