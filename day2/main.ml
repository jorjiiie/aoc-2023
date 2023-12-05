open String
open List

let read_file file = 
  let lines = ref [] in
  let ic = open_in file in 
  try 
    while true; do 
      lines := input_line ic :: !lines 
    done; !lines 
  with End_of_file -> 
    close_in ic;
    List.rev !lines 

let is_digit c = c <= '9' && c >= '0'

let rec print_l = function
  | [] -> ()
  | [x] -> Printf.printf "%s\n" x
  | x :: xs ->
      Printf.printf "%s || " x;
      print_l xs

let rec print_pair = function 
  | [] -> () 
  | [x] -> Printf.printf "%d %s\n" (fst x) (snd x)
  | x :: xs -> 
      Printf.printf "%d %s || " (fst x) (snd x);
      print_pair xs


let split_pair str =
  let l = split_on_char ' ' (trim str) in 
  (int_of_string (hd l), (hd (tl l)))

let rec search_col col pairs =
  match pairs with
  | [] -> 0
  | (n, color) :: xs -> if col = color then n else search_col col xs

let validate pairs = 
  (search_col "red" pairs <= 12) && 
  (search_col "green" pairs <= 13) && 
  (search_col "blue" pairs <= 14)
  


let rec andmap pred l = 
  match l with 
  | [] -> true
  | x :: xs -> if pred x then andmap pred xs else false

(* Map over all lines and just keep a running list of the numbers *)

let solve str = 
  let a = split_on_char ':' str in 
  let b = split_on_char ';' (hd (tl a)) in 
  let c = map (fun x -> map (fun y -> trim y) (split_on_char ',' x)) b in
  let d = map (fun x -> map (fun y -> split_pair y) x) c in 
  andmap validate d


let fi (a, _, _) = a 
let se (_, b, _) = b
let trd (_, _, c) = c

let rec get_max (pairs : (int * string) list) (rgb : (int * int * int))  =
  match pairs with 
  | [] -> rgb
  | (n, color) :: rest ->
      let r = fi rgb in 
      let g = se rgb in 
      let b = trd rgb in
      match color with 
      | "red" -> get_max rest (n, g, b)
      | "green" -> get_max rest (r, n, b)
      | "blue" -> get_max rest (r, g, n)
      | _ -> (r,g,b)


let max_tup a b = 
  (max (fi a) (fi b), max (se a) (se b), max (trd a) (trd b))

let mxpairs lop = 
  let a = map (fun x -> get_max x (0, 0, 0)) lop in 
  List.fold_left (max_tup) (0,0,0) a

let mul tup = 
  (fi tup) * (se tup) * (trd tup)


let solve2 str = 
  let a = split_on_char ':' str in 
  let b = split_on_char ';' (hd (tl a)) in 
  let c = map (fun x -> map (fun y -> trim y) (split_on_char ',' x)) b in
  let d = map (fun x -> map (fun y -> split_pair y) x) c in (* this gives us the pairs*)
  mul (mxpairs d)


let file = "in.txt"
let a = read_file file


let rec accum lst indx = 
  match lst with 
  | [] -> 0
  | x :: xs -> if solve x then indx + accum xs (indx + 1) else accum xs (indx + 1)
let () = 
  Printf.printf "ans is %d\n" (accum a 1)

let () = 
  let z = List.fold_left (fun accum x -> accum + (solve2 x)) 0 a in 
  Printf.printf "ans2 is %d\n" z


