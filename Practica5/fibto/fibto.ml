let rec fib n =
  if n <= 1 then n
  else fib (n-1) + fib (n-2)

let print_fib_series n =
  let rec print_fib i =
    if i <= n then begin
      Printf.printf "%d\n" (fib i);
      print_fib (i+1)
    end
  in
  print_fib 0

(*
let main () =
  if Array.length Sys.argv <> 2 then
    Printf.eprintf "fibto: Invalid number of arguments\n"
  else
    let n = int_of_string Sys.argv.(1) in
    print_fib_series n

let () = main ()
*)