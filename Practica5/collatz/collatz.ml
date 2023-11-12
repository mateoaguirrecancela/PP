let rec f n =
  if n mod 2 = 0 then n / 2 else 3 * n + 1

(*camino que se sigue desde ese número hasta el 1*)
let rec orbit n =
  if n = 1 then
    "1"
  else
    let next = f n in
    let orbit_str = orbit next in
    string_of_int n ^ ", " ^ orbit_str

(*numero de pasos necesarios para llegar hasta el 1*)
let rec length n =
  if n = 1 then 0
  else 1 + length (f n)

(*valor mas alto alcanzado en la orbita*)
let rec top n =
  if n = 1 then 1
  else max n (top (f n))

(*par de enteros indicando la longitud de su orbita y su altura maxima*)
let rec length'n'top n =
  if n = 1 then (0, 1)
  else
    let (len, top_val) = length'n'top (f n) in
    (1 + len, max n top_val)

(*menor valor del intervalo [m, n] cuya orbita tenga longitud maximal en ese intervalo, acompañado de dicha logitud maximal*)
let rec longest_in m n =
  if m > n then
    failwith "Intervalo incorrecto"
  else if m = n then
    (m, length (f m))
  else
    let (val_m, len_m) = length'n'top m in
    let (val_n, len_n) = length'n'top n in
    if len_m >= len_n then
      (m, len_m)
    else
      longest_in (m + 1) n

(*menor valor del intervalo [m, n] cuya orbita tenga altura maximal en ese intervalo, acompañado de dicha altura maximal*)
let rec highest_in m n =
  if m > n then
    failwith "Intervalo incorrecto"
  else if m = n then
    (m, top m)
  else
    let (val_m, top_m) = length'n'top m in
    let (val_n, top_n) = length'n'top n in
    if top_m >= top_n then
      (m, top_m)
    else
      highest_in (m + 1) n