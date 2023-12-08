open String 
open List
open Printf

module type Map = sig 

  type ('k, 'v) t
  (* modifies if k is already in there *)
  val insert : 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t
  
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

module type Set = sig
  type 'k t
  val insert : 'k -> 'k t -> 'k t
  val append : 'k t -> 'k t -> 'k t


  val to_list : 'k t -> 'k list
  val empty : 'k t
end

module ISet : Set = struct 
  type 'k t = 'k list 

  let insert k_ m_ = 
    let rec ins k m = 
      match m with 
      | [] -> [k]
      | x :: rst -> if x = k then x :: rst else ins k rst
    in 
  ins k_ m_

  let append m1 m2 = 
    let rec ins m lst = 
      match m with 
      | [] -> lst 
      | x :: rst -> ins rst (insert x lst)
    in 
  ins m1 m2

  let to_list m =
    let rec loop = function 
      | [] -> []
      | x :: xs -> x :: loop xs
    in 
    loop m

  let empty = []

end




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
let get_digit c = int_of_char c - int_of_char '0'

type grid = char list list

let is_symb c = 
  not (is_digit c) && c != '.'

let in_bound g r c = 
  if r < 0 || c < 0 || (r >= length g) || (c >= length (hd g)) then false else true

let get_el g r c = 
  if in_bound g r c then List.nth (List.nth g r) c
  else '.'

let rec andmap pred = function 
  | [] -> true 
  | x :: xs -> if pred x then andmap pred xs else false

let rec ormap pred = function 
  | [] -> false 
  | x :: xs -> if pred x then true else ormap pred xs

let check_adj g r c fn =
  let neighbors = 
    List.concat_map (fun i ->
      List.map (fun j -> get_el g (r+i) (c+j)) 
      [-1;0;1]) 
    [-1;0;1]
  in 
  ormap fn neighbors



let printx (x : int * (int * int) list) = 
  printf "[%d: {" (fst x);
  iter (fun x -> printf "(%d, %d) " (fst x) (snd x)) (snd x);
  printf "]\n"

  (*take a list of char list list -> (char * (int * int)) list list
   then filter this then do whatever *)
let solve strs = 
  let rec get_tups lst r = 
    let rec inner_rec lst2 c =
      match lst2 with
    | [] -> []
    | x :: xs -> (x, (r,c)) :: inner_rec xs (c+1)
  in 
  match lst with 
  | [] -> []
  | x :: xs -> (inner_rec x 0) :: (get_tups xs (r+1))
  in
  let get_y tup = (fst (snd tup)) in 
  let get_x tup = (snd (snd tup)) in
  let rec fold_lst (lst : ('a * (int * int)) list) (acc : (int * (int * int) list)) =
    match lst with 
    | [] -> [acc]
    | x :: rst ->
        let prev_col = (snd (hd (snd acc))) in 
        if prev_col = get_x x - 1 then 
          fold_lst rst ((fst acc) * 10 + (get_digit (fst x)),
          snd x :: snd acc)
        else acc :: fold_lst rst (get_digit (fst x), [(snd x)])
  in
  let a = (get_tups strs 0) |> map (fun x -> filter (fun y -> is_digit (fst y)) x) in
  let y = flatten (map (fun x -> fold_lst x (0, [(0,0)])) a) in
  y
 

let f = "in.txt"
let a = map (fun x ->  of_seq (String.to_seq x)) (read_file f)
      

let print_tup tup = 
  printf "%c at [%d %d] " (fst tup) (fst (snd tup)) (snd (snd tup))
(* get tuples of (value, list of coords) *)


let stars (r : int) (c : int) : (int * int) ISet.t = 
  let neighbors = 
    List.concat_map (fun i ->
      List.map (fun j -> ((r+i), (c+j)))
      [-1;0;1]) 
    [-1;0;1]
  in 
  let coords : (int * int) ISet.t = ISet.empty in
  let rec loop lst m = 
    match lst with 
    | [] -> m
    | x :: rst -> 
        let y = get_el a (fst x) (snd x) in 
        if y = '*' then loop rst (ISet.insert x m) else loop rst m
  in 
  loop neighbors coords

let rec map_insert mp lst v = 
  match lst with 
  | [] -> mp
  | x :: rst -> 
      let y = IMap.get x mp in 
      match y with 
      | None -> map_insert (IMap.insert x [v] mp) rst v
      | Some(z) -> map_insert (IMap.insert x (v :: z) mp) rst v

  
(* we are given the list of (int, coord list) and just map over the ones 
 that have a star in them*)
let part_2 (nums : (int * (int * int) list) list) = 
  let mp  = IMap.empty in 
  let rec fold_val lst m =
    match lst with 
    | [] -> m 
    | x :: rst -> 
        let star_coords = fold_left (fun accum x -> ISet.append x accum) ISet.empty (map (fun y -> stars (fst y) (snd y)) (snd x)) in
        (* insert (fst x) into all of star coords in the map *)
        fold_val rst (map_insert m (ISet.to_list star_coords) (fst x)) 
  in 
  fold_val nums mp

  
  




let () = let b = solve a in 
printf "%d\n" (length b);
let y = filter (fun x -> (fst x) <> 0 && ormap (fun y -> check_adj a (fst y) (snd y) is_symb) (snd x)) b
in printf "%d\n" (length y);
let ans = fold_left (fun a b -> a + b) 0 (map (fun x -> (fst x)) y) in 
printf "%d\n" ans

let c = part_2 (solve a)
let d = filter (fun x -> 2 = (length (snd x))) (IMap.to_list c)
    |> map (fun x -> (hd (snd x)) * (hd (tl (snd x))))
    |> fold_left (fun a b -> a + b) 0

let () = 
  printf "%d\n" d
