open Printf
open List

let read_file f =
  let ic = open_in f in
  let lines = ref [] in
  try
    while true; do
      lines := input_line ic :: !lines
    done; !lines
  with End_of_file ->
    close_in ic;
    rev !lines

let rec andmap fn = function
  | [] -> true 
  | h :: t -> if fn h then andmap fn t else false

let transpose lst = 
  let rec help acc = function 
    | [] :: _ -> rev acc
    | m -> help (map hd m :: acc) (map tl m)
  in 
  help [] lst


let rec pmtx = function 
  | [] -> printf "\n";
  | h :: t -> printf "[";
  iter (printf "%c") h; printf "]\n"; pmtx t

let grid = read_file "in.txt" |> map (fun x -> of_seq (String.to_seq x))
let tgrid = transpose grid

let bad_col c = andmap (fun x -> x = '.') (nth tgrid c)

let bad_row r = andmap (fun x -> x = '.') (nth grid r)

let coords = 
  let rec get_within lst idx1 idx2 = 
    match lst with 
    | [] -> []
    | h :: t -> if h = '#' then (idx1, idx2) :: get_within t idx1 (idx2+1) else get_within t idx1 (idx2+1)
  in 
  let rec loop lst idx = 
    match lst with 
    | [] -> []
    | h :: t -> append (get_within h idx 0) (loop t (idx+1))
  in 
  loop grid 0

let p1 = 
  let dist2 lst = 
    fold_left (fun a b -> a + if b then 2 else 1) 0 lst 
  in 
  let dist (a,b) (c,d) = 
    let (sx, sy) = (min a c, min b d) in 
    let (ex, ey) = (max a c, max b d) in 
    let l1 = init (ex-sx) (fun x -> x + sx) in 
    let l2 = init (ey-sy) (fun x -> x + sy) in 
    (dist2 (map bad_row l1)) + (dist2 (map bad_col l2))
  in 
  let rec calc_to c lst =
    match lst with 
    | [] -> 0
    | h :: t -> (dist c h) + calc_to c t
  in 
  let rec help lst = 
    match lst with 
    | [] -> 0
    | h :: t -> (calc_to h t) + help t
  in 
  help coords

let () = printf "part1: %d\n" p1

let p2 = 
  let dist2 lst = 
    fold_left (fun a b -> a + if b then 1000000 else 1) 0 lst 
  in 
  let dist (a,b) (c,d) = 
    let (sx, sy) = (min a c, min b d) in 
    let (ex, ey) = (max a c, max b d) in 
    let l1 = init (ex-sx) (fun x -> x + sx) in 
    let l2 = init (ey-sy) (fun x -> x + sy) in 
    (dist2 (map bad_row l1)) + (dist2 (map bad_col l2))
  in 
  let rec calc_to c lst =
    match lst with 
    | [] -> 0
    | h :: t -> (dist c h) + calc_to c t
  in 
  let rec help lst = 
    match lst with 
    | [] -> 0
    | h :: t -> (calc_to h t) + help t
  in 
  help coords

let () = printf "part2 : %d\n" p2
