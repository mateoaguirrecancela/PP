(*let g n = (n >= 0 && n mod 2 = 0) || n mod 2 = -1;;*)

(*sin && ni ||*)
let g1 n =
  if n >= 0 then
    if n mod 2 = 0 then
      true
    else
      false
  else
    if n mod 2 = -1 then
      true
    else
      false;;

(*sin && ni || ni if-then-else*)
let g2 n =
  match n with
  | n when (n >= 0, n mod 2 = 0) -> true
  | n when (n mod 2 = -1) -> true
  | _ -> false;;