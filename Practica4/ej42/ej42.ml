(*redefinir la función min*)
let min a b = if a < b then a else b;;

(*redefinir la función max*)
let max a b = if a > b then a else b;;

(*redefinir la función fst*)
let fst (a, _) = a;;

(*redefinir la función snd*)
let snd (_, b) = b;;