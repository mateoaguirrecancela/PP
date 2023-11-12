let rec fib n =
  if n <= 1 then n
  else fib (n-1) + fib (n-2)

let fibto n =
  let rec print_fib i =
    if i <= n then begin
      Printf.printf "%d\n" (fib i);
      print_fib (i+1)
    end
  in
  print_fib 0