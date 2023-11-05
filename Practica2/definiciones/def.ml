(*funcion predefinida*)
let v = int_of_string "42";;

(*operadores infijos*)
let w = 5.0 +. 2.0 *. 3.0 -. 1.0 /. 4.0;;

(*frase if-then-else*)
let x = if v > 0 then 'A' else 'B';;

(*operadores logicos*)
let y = (v > 0) || (w < 10.0);;

(*sub-expresion*)
let z =
  let sub_expression = string_of_int v in
  "El valor de v es " ^ sub_expression;;