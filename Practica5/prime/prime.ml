(* Función para determinar si un número es primo *)
let is_prime n =
  let rec check_from i =
    i >= n || (n mod i <> 0 && check_from (i+1))
  in
  check_from 2;;

(* Función para encontrar el primer número primo mayor que n *)
let next_prime n =
  let rec find_prime m =
    if is_prime m then
      m
    else
      find_prime (m+1)
  in
  find_prime (n+1);;

(* Función para encontrar el mayor primo menor o igual que n *)
let last_prime_to n =
  let rec find_prime m =
    if is_prime m && m <= n then
      m
    else if m > 2 then
      find_prime (m-1)
    else
      failwith "No se encontro un primo en el rango dado"
  in
  find_prime n;;

(* Función is_prime2 más eficiente *)
let is_prime2 n =
  if n <= 1 then
    false
  else if n <= 3 then
    true
  else if n mod 2 = 0 || n mod 3 = 0 then
    false
  else
    let rec check_from i =
      i * i > n || (n mod i <> 0 && check_from (i+6))
    in
    check_from 5;;