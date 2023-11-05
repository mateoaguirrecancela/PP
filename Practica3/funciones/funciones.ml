let rec factorial = function 0 -> 1 | n -> n * factorial (n-1);;

factorial 0 + factorial 1 + factorial 2;;
(*int = 4*)

factorial 10;;
(*int = 3628800*)

factorial 100;;
(*int = 0*)

factorial (-1);;
(*Error de Ejecucion, no se puede hacer el factorial de numeros negativos*)



(*suma de todos los naturales hasta el n*)
let rec sumto = function 0 -> 0 | n -> n + sumto (n-1);;

(*valor de 10^n*)
let rec exp10 = function 0 -> 1 | n -> 10 * exp10(n-1);;

(*número de cifras de la representacion decimal de n*)
let rec num_cifras = function | 0 -> 1 | n -> 1 + num_cifras(abs(n)/10);;

(*suma de las cifras correspondientes a la representacion decimal de n*)
let rec sum_cifras = function 0 -> 0 | n -> (abs(n) mod 10) + sum_cifras (abs(n)/10);;