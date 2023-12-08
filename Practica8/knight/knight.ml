(* knight.ml *)

exception Not_found

(* Función auxiliar para verificar si una casilla está dentro del tablero *)
let dentro_del_tablero m n (x, y) =
  x >= 0 && x < m && y >= 0 && y < n

(* Función auxiliar que devuelve los movimientos posibles de un caballo en un tablero 2D *)
let movimientos_caballo = [(-2, -1); (-2, 1); (-1, -2); (-1, 2); (1, -2); (1, 2); (2, -1); (2, 1)]

(* Función auxiliar para filtrar los movimientos válidos dado un tablero y una posición *)
let movimientos_validos m n obstaculos (x, y) =
  List.filter
    (fun (dx, dy) ->
      let nueva_pos = (x + dx, y + dy) in
      dentro_del_tablero m n nueva_pos && not (List.mem nueva_pos obstaculos))
    movimientos_caballo

(* Función auxiliar para realizar el recorrido del caballo *)
let rec recorrer_caballo m n obstaculos actual destino recorrido =
  if actual = destino then
    recorrido @ [actual]
  else
    let movimientos_posibles = movimientos_validos m n obstaculos actual in
    match movimientos_posibles with
    | [] -> raise Not_found
    | _ ->
        let rec intentar_movimiento = function
          | [] -> raise Not_found
          | siguiente :: rest ->
              let nueva_lista = recorrido @ [actual] in
              try
                recorrer_caballo m n obstaculos siguiente destino nueva_lista
              with Not_found ->
                intentar_movimiento rest
        in
        intentar_movimiento movimientos_posibles

(* Función principal tour *)
let tour m n obstaculos ini fin =
  let _ =
    if not (dentro_del_tablero m n ini && dentro_del_tablero m n fin) then
      raise Not_found
  in
  recorrer_caballo m n obstaculos ini fin []

(* Función principal min_tour *)
let min_tour m n obstaculos ini fin =
  let rec min_tour_aux recorrido mejor_recorrido =
    if recorrido = [] then
      mejor_recorrido
    else
      let longitud_recorrido = List.length recorrido in
      let longitud_mejor_recorrido = List.length mejor_recorrido in
      if longitud_recorrido < longitud_mejor_recorrido || mejor_recorrido = [] then
        min_tour_aux (List.tl recorrido) recorrido
      else
        min_tour_aux (List.tl recorrido) mejor_recorrido
  in
  let resultado = tour m n obstaculos ini fin in
  min_tour_aux resultado []

(* Función principal min_tour4D *)
let min_tour4D m n obstaculos ini fin =
  let rec min_tour4D_aux recorrido mejor_recorrido =
    if recorrido = [] then
      mejor_recorrido
    else
      let longitud_recorrido = List.length recorrido in
      let longitud_mejor_recorrido = List.length mejor_recorrido in
      if longitud_recorrido < longitud_mejor_recorrido || mejor_recorrido = [] then
        min_tour4D_aux (List.tl recorrido) recorrido
      else
        min_tour4D_aux (List.tl recorrido) mejor_recorrido
  in
  let resultado = tour m n obstaculos ini fin in
  min_tour4D_aux resultado []
