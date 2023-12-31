let append arr1 arr2 =
  let len1 = Array.length arr1 in
  let len2 = Array.length arr2 in
  let result = Array.make (len1 + len2) arr1.(0) in
  for i = 0 to len1 - 1 do
    result.(i) <- arr1.(i);
  done;
  for i = 0 to len2 - 1 do
    result.(len1 + i) <- arr2.(i);
  done;
  result

let sub arr start len =
  let result = Array.make len arr.(0) in
  for i = 0 to len - 1 do
    result.(i) <- arr.(start + i);
  done;
  result

let copy arr =
  let len = Array.length arr in
  let result = Array.make len arr.(0) in
  for i = 0 to len - 1 do
    result.(i) <- arr.(i);
  done;
  result

let fill arr start len value =
  for i = start to start + len - 1 do
    arr.(i) <- value;
  done

let blit src src_pos dest dest_pos len =
  for i = 0 to len - 1 do
    dest.(dest_pos + i) <- src.(src_pos + i);
  done

let to_list arr =
  let len = Array.length arr in
  let result = ref [] in
  for i = len - 1 downto 0 do
    result := arr.(i) :: !result;
  done;
  !result

let iter f arr =
  let len = Array.length arr in
  for i = 0 to len - 1 do
    f arr.(i);
  done

let fold_left f acc arr =
  let len = Array.length arr in
  let result = ref acc in
  for i = 0 to len - 1 do
    result := f !result arr.(i);
  done;
  !result

let for_all pred arr =
  let len = Array.length arr in
  let result = ref true in
  for i = 0 to len - 1 do
    if not (pred arr.(i)) then result := false;
  done;
  !result

let exists pred arr =
  let len = Array.length arr in
  let result = ref false in
  for i = 0 to len - 1 do
    if pred arr.(i) then result := true;
  done;
  !result

let find_opt pred arr =
  let len = Array.length arr in
  let result = ref None in
  for i = 0 to len - 1 do
    if pred arr.(i) then result := Some arr.(i);
  done;
  !result
