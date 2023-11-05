(* Función para invertir el orden de las cifras de un número *)
let rec reverse n =
  if n < 10 then n  (* Si n tiene una sola cifra, devuelve n *)
  else
    let last_digit = n mod 10 in
    let rest = n / 10 in
    let reversed_rest = reverse rest in
    let length = int_of_float (log10 (float_of_int reversed_rest)) + 1 in
    last_digit * int_of_float (10. ** float_of_int length) + reversed_rest;;

(* Función para verificar si un string es un palíndromo *)
let rec palindromo s =
  let len = String.length s in
  if len <= 1 then true  (* Si la longitud es 0 o 1, es un palíndromo *)
  else if s.[0] = s.[len - 1] then
    palindromo (String.sub s 1 (len - 2))  (* Compara el primer y último carácter y sigue verificando el interior *)
  else false;;

(* Función para calcular el máximo común divisor (MCD) de dos enteros x e y *)
let rec mcd (x, y) =
  if y = 0 then x
  else mcd (y, x mod y);;