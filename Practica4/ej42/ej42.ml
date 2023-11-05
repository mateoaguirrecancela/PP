(* Redefinir la función min *)
let min a b = if a < b then a else b;;

(* Redefinir la función max *)
let max a b = if a > b then a else b;;

(* Redefinir la función fst *)
let fst (a, _) = a;;

(* Redefinir la función snd *)
let snd (_, b) = b;;