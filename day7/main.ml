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


exception CustomException of string

let order_char = "AKQJT98765432"

let char_comp a b = 
  let rec help idx = 
    if idx >= 13 then raise (CustomException "joe mama")
    else if a = order_char.[idx] then 1 else 
      if b = order_char.[idx] then -1 else 
        help (idx+1)
  in 
  if a = b then 0 else help 0


let sort_string_lexicographically str =
  let char_list = List.of_seq (String.to_seq str) in
  let sorted_list = List.sort Char.compare char_list in
  String.of_seq (List.to_seq sorted_list)

(* these all take strings *)
let is_five hand = 
  let arr = sort_string_lexicographically hand in 
  arr.[0] = arr.[1] && arr.[1] = arr.[2] && arr.[2] = arr.[3] && arr.[3] = arr.[4]


let is_four hand = 
  let arr = sort_string_lexicographically hand in 
  (arr.[0] = arr.[1] && arr.[1] = arr.[2] && arr.[2] = arr.[3])
  || (arr.[1] = arr.[2] && arr.[2] = arr.[3] && arr.[3] = arr.[4])

let is_full hand = 
  let arr = sort_string_lexicographically hand in 
  (arr.[0] = arr.[1] && arr.[2] = arr.[3] && arr.[3] = arr.[4]) ||
  (arr.[0] = arr.[1] && arr.[1] = arr.[2] && arr.[3] = arr.[4])

let is_three hand = 
  let arr = sort_string_lexicographically hand in 
  (arr.[0] = arr.[1] && arr.[1] = arr.[2]) || 
  (arr.[1] = arr.[2] && arr.[2] = arr.[3]) ||
  (arr.[2] = arr.[3] && arr.[3] = arr.[4])


let count_pairs str = 
  let arr = sort_string_lexicographically str in
  snd (fold_left (fun (a, b) c -> if c = a then (c, b + 1) else (c, b)) ('z', 0) (List.of_seq(String.to_seq arr)))

let is_two hand = 
  count_pairs hand = 2

let is_one hand = 
  count_pairs hand = 1


let high hand = true


let comp_same a b = 
  let rec help idx = 
    if idx >= 5 then raise (CustomException "no bueno x2")
    else 
      let res = char_comp a.[idx] b.[idx] in 
      if res = 0 then help (idx+1) else res
  in 
  help 0
    
let types = [is_five;is_four;is_full;is_three;is_two;is_one;high]

(* if a < b *)
let comp a b = 
  let rec c_help cur x y = 
    match cur with 
    | [] -> 
        printf "whats going on %s %s\n" a b;
        raise (CustomException "no bueno!")
    | first :: rest -> 
        let res_a = first x in 
        let res_b = first y in 
        if res_a && res_b then comp_same x y else
          if res_a then 1 else
            if res_b then -1 else 
              c_help rest x y
  in 
  if a = b then 0 else c_help types a b
    

  

let split_inp str =
  match split_on_char ' ' str with 
  | [a; b] -> (a,int_of_string b)
  | _ -> failwith "nooooo"


let rec accum lst idx = 
  match lst with 
  | [] -> 0
  | (str, bid) :: rest -> bid * idx + (accum rest (idx+1))



let inp = read_file "in.txt"
    |> map split_inp 
    |> sort (fun a b -> (comp (fst a) (fst b)))

let () = iter (fun x -> printf "%s :: %d\n" (fst x) (snd x)) inp
let () = printf "part 1 = %d\n" (accum inp 1)



let () = printf "%d" (comp_same "KK677" "KTJJT")
