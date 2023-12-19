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
    rev !lines 


type entry =
  { start: int;
    len: int;
    from: int;
  }

(* where is a in relation to b *)
type status = Surround | Left | LeftEdge | Middle | RightEdge | Right 

let comp a b = 
  let aend = a.start + a.len in 
  let bend = b.start + b.len in
  if a.start < b.start then 
    begin
    if aend <= b.start then Left else 
      if aend < bend then LeftEdge else Surround
    end
  else 
    begin 
      if bend <= a.start then Right else 
        if bend < aend then RightEdge else Middle
    end

(* chop a off of b *)
let chop_right a b = 
  {b with len = a.start - b.start}

(* chop a off b but on the left this time*)
let chop_left a b = 
  {start = a.start + a.len; 
  len = b.start + b.len - a.start - a.len;
  from = b.from + (a.start + a.len - b.start)}

let inside ent x = 
  x >= ent.start && x < ent.start + ent.len
module Translation = 
  struct  
    let empty = []
    let rec add ent m = 
      match m with
      | [] -> [ent]
      | h :: t -> 
          begin 
            match comp ent h with 
            | Surround -> add ent t
            | Left -> ent :: m
            | LeftEdge -> 
                ent :: chop_left ent h :: t
            | Middle -> (* chop h up *)
                let a = chop_right ent h in 
                let b = chop_left ent h in 
                let cons_empty x y = 
                  if x.len = 0 then y 
                  else x :: y
                in 
                cons_empty a (cons_empty ent (cons_empty b t))
            | RightEdge -> 
                chop_right ent h :: add ent t
            | Right -> h :: add ent t
          end

    let rec search x m = 
      match m with 
      | [] -> x
      | h :: t -> 
          if x < h.start then x else
            if x >= h.start + h.len then search x t else 
              h.from + x - h.start
  end

let x = {start=1;len=5;from=69}
let y = {start=5;len=5;from=69}
let z = {start=2;len=1;from=420}

 
let inp_raw = read_file "in.txt"

type line = NewMap | Entry of entry

let rec get_seeds lst = 
  match lst with 
  | [] -> []
  | a :: b :: t -> (a,b) :: get_seeds t
  | _ -> failwith "odd??"

let seeds = inp_raw |> hd |> split_on_char ':' |> tl |> hd |> trim |> split_on_char ' ' |> map (int_of_string)
let seeds2 = get_seeds seeds

let proc_ops lst_ = 
  let rec help lst = 
    match lst with 
    | [] -> [Translation.empty]
    | h :: t -> begin 
      let rst = help t in
      match h with 
      | NewMap -> Translation.empty :: rst
      | Entry e -> Translation.add e (hd rst) :: (tl rst)
    end
  in 
  help lst_

let inp = rev (tl (tl inp_raw))

let proc_inp inp_list = 
  let rec help lst = 
    match lst with 
    | [] -> []
    | h :: t -> begin 
      match h with 
      | "" -> Some NewMap
      | str -> 
          begin 
            let y = split_on_char ' ' str in 
            if length y = 2 then None 
            else 
              let x = map int_of_string y in 
              match x with 
              | [a;b;c] -> Some (Entry {start=b;from=a;len=c})
              | _ -> failwith "some string is weird"
          end
    end :: help t
  in 
  map Option.get (filter Option.is_some (help inp_list))


let op_list = proc_inp inp

let print_line = function 
  | NewMap -> "new map!"
  | Entry {start=a;from=b;len=c} -> sprintf "%d %d %d" a b c


(* let () = iter (fun x -> printf "line is :%s\n" (print_line x)) op_list *)

let translators = rev (proc_ops op_list)

let translate_coord x = 
  let rec help lst y = 
    match lst with 
    | [] -> y 
    | h :: t -> 
      help t (Translation.search y h)
  in 
  help translators x


let zz = map translate_coord seeds
let () = printf "part1 = %d\n%!" (fold_left (fun a b -> min a b) max_int zz)

let rec part2 lst = 
  let try_all start nd = 
    let x = ref max_int in
    for v = start to (nd-1) do 
      x := min !x (translate_coord v)
    done; !x
  in
  match lst with 
  | [] -> max_int 
  | (a,b) :: t ->
      printf "starting comp for [%d,%d) which is %d numbers\n%!" a (a+b) b;
      let zz = try_all a (a+b) in 
      printf "finished this comp with %d on range [%d, %d)\n%!" zz a (a+b);
      min zz (part2 t)

let () = printf "part 2 = %d\n" (part2 seeds2)


