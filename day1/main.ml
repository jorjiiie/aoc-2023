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
  

let nums = ["one";"two";"three";"four";"five";"six";"seven";"eight";"nine";]


let prefix pref str =
  if String.length pref > String.length str then false else
  let pref_len = String.length pref in 
  let first_half = String.sub str 0 pref_len in 
  pref = first_half 

let assoc_nums str = 
  let rec fn cur s n =
    match cur with 
    | [] -> None
    | num :: rest -> if prefix num s then Some(string_of_int n) else fn rest s (n+1)
  in 
  let get_digit str =
    match str with
    | "" -> None 
    | s -> if is_digit s.[0] then Some(String.make 1 s.[0]) else fn nums s 1
  in
  let rec replace_all str =
    match str with
    | "" -> []
    | s -> get_digit s :: replace_all (String.sub str 1 (String.length str - 1))
  in 
  replace_all str


let rec print_l = function
  | [] -> ()
  | [x] -> Printf.printf "%s\n" x 
  | x :: xs -> 
      Printf.printf "%s; " x;
      print_l xs
  
let print_option_list lst =
  let print_option = function
    | Some value -> Printf.printf "Some %s; " value
    | None -> Printf.printf "None; "
  in
  List.iter print_option lst;
  Printf.printf "\n"
;;
let a = read_file file
let () = 
  let b = List.map (fun x -> int_of_string (get_concat x)) a in
  let c = List.fold_left (fun acc n -> acc + n) 0 b in
  Printf.printf "%d\n" c;

  let b = List.map (fun x -> assoc_nums x) a in 
  let c = List.map (fun x -> (List.filter (fun y -> Option.is_some y) x)) b in
  let d = List.map (fun x -> (List.map (fun y -> Option.get y) x)) c in
  let e = List.map (fun x -> String.concat "" x) d in
  let f = List.map (fun x -> int_of_string (get_concat x)) e in 
  let g = List.fold_left (fun acc n -> acc + n) 0 f in 
  Printf.printf "%d\n" g;


 

