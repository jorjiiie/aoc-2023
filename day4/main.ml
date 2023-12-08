open Printf 
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


let rec find l1 el = 
  match l1 with
    | [] -> false
    | x :: rst -> if el = x then true else find rst el

let rec pow a = function
  | 0 -> 1 
  | n -> a * (pow a (n-1))

let part1 (str : string) = 
  let process (x : string) : int list = 
    split_on_char ' ' x |> filter (fun x -> x <> "") |> map int_of_string 
  in
  let b = trim (hd (tl (split_on_char ':' str))) 
  |> split_on_char '|' 
  |> List.map process in 
  let c = (hd b) in 
  let d = (hd (tl b)) in 
  let e = filter (fun x -> find c x) d
  |> length 
  |> pow 2
  in e/2


let arr = Array.make 269 1
let cur = Array.make 1 0
let ans = Array.make 1 0

let rec go start cnt v = 
  match cnt with 
  | 0 -> ()
  | n -> arr.(start) <- (arr.(start)) + v;
  go (start + 1) (cnt - 1) v

let part2 (str : string) : int = 
  let process (x : string) : int list = 
    split_on_char ' ' x |> filter (fun x -> x <> "") |> map int_of_string 
  in
  let b = trim (hd (tl (split_on_char ':' str))) 
  |> split_on_char '|' 
  |> List.map process in 
  let c = (hd b) in 
  let d = (hd (tl b)) in 
  let e = filter (fun x -> find c x) d 
  |> length in 
  go (cur.(0)+1) e arr.(cur.(0));
  printf "currently on %d %d\n" cur.(0) arr.(cur.(0));
  ans.(0) <- ans.(0) + arr.(cur.(0));
  cur.(0) <- cur.(0) + 1;
  69



let () = printf "%d " (part1 "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53")

let () = printf "%d " (part1 "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19")


let a = read_file "in.txt"

let () = iter (printf "%s\n") a


let () = printf "%d " (fold_left (fun a b -> a + b) 0 (map part1 a))

let f = map part2 a

let () = printf "%d " ans.(0)
