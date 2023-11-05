(* power : int -> int -> int
   Esta función calcula x^y utilizando recursión y la propiedad xy = x * xy-1 *)
let rec power x y =
  if y = 0 then 1
  else x * power x (y - 1);;

(* power' : int -> int -> int
   Esta función mejora la eficiencia utilizando la propiedad xy = (x * x)^(y/2).
   Si y es par, utiliza esta propiedad. Si y es impar, utiliza xy = x * (x * x)^(y/2) *)
let rec power' x y =
  if y = 0 then 1
  else if y mod 2 = 0 then
    let half_power = power' x (y / 2) in
    half_power * half_power
  else
    x * power' x (y - 1);;

(* powerf : float -> int -> float
   Esta función calcula x^n para números de punto flotante utilizando recursión *)
let rec powerf x n =
  if n = 0 then 1.0
  else x *. powerf x (n - 1);;