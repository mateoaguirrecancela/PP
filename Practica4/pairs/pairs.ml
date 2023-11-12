(*obtener el siguiente valor en el recorrido*)
let next (x, y) =
  if x = 1 then                           (*casos de cambio de direccion*)
    if y mod 2 = 0 then (x+1,y-1)
    else (x,y + 1)
  else
    if y = 1 then
    if x mod 2 = 0 then (x+1,1)
    else (x - 1,y + 1)
    else
      if (x+y) mod 2 = 0 then (x-1,y+1)   (*si la suma es par, el siguiente valor es la diagonal de abajo a la izquierda*)
      else (x+1,y-1)

(*avanzar n pasos desde un punto dado*)
let rec steps_from (x, y) n =
  if n = 0 then (x, y)
  else steps_from (next (x, y)) (n - 1)

(*representar la funcion pair*)
let rec pair n =
  steps_from (1, 1) (n - 1)