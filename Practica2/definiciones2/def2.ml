(*perímetro de una circunferencia con radio r*)
let p = function r -> 2.0 *. 3.14159265 *. r

(*area de un circulo con radio r*)
let area = function r -> 3.14159265 *. r *. r

(*valor absoluto*)
let absf = function x -> if x < 0.0 then -.x else x

(*numero par*)
let even = function x -> x mod 2 = 0

(*menor multiplo de 3 mayor o igual que un numero*)
let next3 = function x -> if x mod 3 = 0 then x else x + (3 - x mod 3)


(*caracter del alfabeto ingles*)
let is_a_letter = function c -> (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

(*convertir booleano en string*)
let string_of_bool = function x -> if x then "verdadero" else "falso"