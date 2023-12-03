let file = "in.txt"

let read_file fname = 
  let lines = ref [] in 
  let ic = open_in fname in 
  try 
    while true; do 
      lines := input_line ic :: !lines 
    done; !lines 
  with End_of_file ->
    close_in ic;
    List.rev !lines

let is_digit c = c <= '9' && c >= '0'


let get_concat str =
  let rest_str str =
    String.sub str 1 (String.length str - 1)
  in 
  let rec match_first str = 
    match str with
    | "" -> Printf.printf "error nothing found??\n"; 'e'
    | _ -> if is_digit str.[0] then str.[0] else match_first (rest_str str)
  in 
  let rec match_last str c = 
    match str with 
    | "" -> c
    | _ -> match_last (rest_str str) (if is_digit str.[0] then str.[0] else c) 
  in 
  (String.make 1 (match_first str)) ^ (String.make 1 (match_last str 'x'))
  


let a = read_file file
let () = 
  let b = List.map (fun x -> int_of_string (get_concat x)) a in
  let c = List.fold_left (fun acc n -> acc + n) 0 b in
  Printf.printf "%d\n" c

