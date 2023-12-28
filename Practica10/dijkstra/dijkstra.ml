exception Invalid_argument of string

type 'a queue = Empty | Node of 'a * 'a queue * 'a queue

let empty = Empty

let is_empty = function
  | Empty -> true
  | _ -> false

let rec merge q1 q2 =
  match q1, q2 with
  | Empty, q | q, Empty -> q
  | Node (x, l1, r1), Node (y, l2, r2) ->
      if x <= y then
        Node (x, merge r1 q2, l1)
      else
        Node (y, merge q1 r2, l2)

let insert x q = merge (Node (x, Empty, Empty)) q

let find_min = function
  | Empty -> raise (Invalid_argument "find_min")
  | Node (x, _, _) -> x

let delete_min = function
  | Empty -> raise (Invalid_argument "delete_min")
  | Node (_, l, r) -> merge l r

let dijkstra w =
  let n = Array.length w in
  if n = 0 || Array.length w.(0) <> n then
    raise (Invalid_argument "dijkstra: Input matrix is not square")
  else
    let dist = Array.make n None in
    let pq = ref empty in
    let rec update_neighbors u =
      for v = 0 to n - 1 do
        match w.(u).(v) with
        | Some weight ->
            let new_dist = match dist.(u) with
              | Some d -> d + weight
              | None -> raise (Invalid_argument "dijkstra: Invalid distance")
            in
            if dist.(v) = None || new_dist < match dist.(v) with
                                                | Some d -> d
                                                | None -> max_int
            then begin
              dist.(v) <- Some new_dist;
              pq := insert (new_dist, v) !pq
            end
        | None -> ()
      done
    in
    for i = 0 to n - 1 do
      match w.(i).(i) with
      | Some _ -> raise (Invalid_argument "dijkstra: Negative weight")
      | None -> ()
    done;
    dist.(0) <- Some 0;
    pq := insert (0, 0) !pq;
    while not (is_empty !pq) do
      let u = snd (find_min !pq) in
      pq := delete_min !pq;
      update_neighbors u
    done;
    Array.map (fun d -> Array.map (fun _ -> d) w.(0)) dist
