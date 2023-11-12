(*power : int -> int -> int*)
(*calcula x^y utilizando recursividad y la propiedad x^y = x * x^(y-1)*)
let rec power x y =
  if y = 0 then 1
  else x * power x (y - 1);;

(*power' : int -> int -> int*)
(*mejora la eficiencia utilizando la propiedad x^y = (x * x)^(y/2)*)
(*si y es par, utiliza esta propiedad. si y es impar, utiliza x^y = x * (x * x)^(y/2)*)
(*esta funcion es mas eficiente ya que realiza la mitad de iteraciones, reduce y a la mitad recursivamente*)
let rec power' x y =
  if y = 0 then 1
  else if y mod 2 = 0 then power' (x*x) (y/2)
    else x * power' (x*x) (y/2);;

(*powerf : float -> int -> float*)
(*calcula x^n para números de punto flotante utilizando recursividad*)
let rec powerf x n =
  if n = 0 then 1.0
  else x *. powerf x (n - 1);;