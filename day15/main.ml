open Printf
open String
open List

let inp = input_line (open_in "in.txt") |> split_on_char ','


let hash str =
  let rec help lst v = begin
    match lst with 
    | [] -> v 
    | h :: t -> help t ((v + (Char.code h)) * 17 mod 256)
  end
  in 
  help (List.of_seq (String.to_seq str)) 0



let res = inp
        |> map hash 
        |> fold_left (fun a b -> a + b) 0 
let () = printf "part 1: %d\n" res

module type Map = sig 

  type ('k, 'v) t
  (* modifies if k is already in there *)
  val insert : 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t

  val erase : 'k -> ('k, 'v) t -> ('k, 'v) t
  
  val get : 'k -> ('k, 'v) t -> 'v option

  val empty : ('k, 'v) t

  val to_list : ('k, 'v) t -> ('k * 'v) list

  val length : ('k, 'v) t -> int
  
end

module IMap : Map = struct 
  type ('k, 'v) t = ('k * 'v) list 
  (* replaces with new value if present *)
  let insert k_ v_ m_ = 
    let rec ins k v m = 
      match m with 
      | [] -> [(k, v)]
      | x :: rst -> 
          if (fst x) = k then
          ((fst x), v) :: rst 
          else x :: ins k v rst
    in 
    ins k_ v_ m_

  let erase k_ m_ = 
    let rec remv k m = 
      match m with 
      | [] -> []
      | (a, b) :: t -> 
          if a = k then t else (a,b) :: remv k t
    in 
    remv k_ m_

  let get (k_ : 'k) (m_ : ('k * 'v) list) = 
    let rec fnd (k : 'k) (m : ('k * 'v) list) = 
      match m with 
      | [] -> None
      | x :: rst -> 
          if (fst x) = k then Some (snd x) else fnd k rst
    in 
    fnd k_ m_

  let rec to_list = function 
    | [] -> []
    | x :: rst -> x :: (to_list rst)
  

  let rec length = function
    | [] -> 0
    | x :: rst -> 1 + (length rst)

  let empty = []
end

 let table = Array.make 256 IMap.empty

let add_item k v = 
  let tbl = hash k in 
  let lst = table.(tbl) in 
  table.(tbl) <- IMap.insert k v lst

let remove_item k = 
  let tbl = hash k in 
  let lst = table.(tbl) in 
  table.(tbl) <-  IMap.erase k lst
  


type op = 
  | Add of string * string 
  | Sub of string

let parse_inp str = 
  let x = split_on_char '-' str in 
  let y = split_on_char '=' str in 
  if length y = 2 then 
    Add ((hd y), (hd (tl y))) else 
      Sub (hd x) 

let do_op = function 
  | Add (k, v) -> add_item k (int_of_string v)
  | Sub k -> remove_item k 


let get_box_focus m = 
  let lst = IMap.to_list m in 
  let rec accum idx cur = 
    match cur with 
    | [] -> 0
    | h :: t -> 
        idx * h + accum (idx+1) t
  in 
  accum 1 (map (fun (a,b) -> b) lst)

let part2 =  lazy(
  let x = ref 0 in
  for k = 0 to 255 do 
    x := !x + get_box_focus table.(k) * (k+1)
  done;
  x)

let () = inp |> map parse_inp |> iter do_op
let p2 = Lazy.force part2
let() = printf "part 2: %d\n" !p2
