open List
open Printf

let read_file f = 
  let ic = open_in f in 
  let lines = ref [] in 
  try 
    while true; do 
      lines := input_line ic :: !lines 
    done; !lines 
  with End_of_file -> 
    close_in ic;
    List.rev !lines

(* returns the polynmial interpolating dp so that you can call it to get another item f(x) *)
    (* lagrange interp *)
let interpolating_poly dp x = 
  let k = (List.length dp) - 1 in
  let basis num n_x = 
    let rec bas_help idx = 
      if idx = (k+1) then 1. else 
        if idx = num then bas_help (idx+1) else 
          (n_x -. (float_of_int idx)) /. ((float_of_int num) -. (float_of_int idx)) *. bas_help (idx+1)
    in 
    bas_help 0
  in 
  let rec lagrange_sum lst idx n_x = 
    match lst with 
    | [] -> 0.
    | h :: t -> h *. (basis idx n_x) +. lagrange_sum t (idx+1) n_x 
  in 
  lagrange_sum dp 0 x


let z = read_file "in.txt" |> map (String.split_on_char ' ') |> map (map float_of_string)

let p1 =
  let solve lst = 
    let k = List.length lst in 
  interpolating_poly lst (float_of_int k)
  in 
  fold_left (fun a b -> a +. b) 0. (map solve z)

let p2 = 
  let solve lst = 
  interpolating_poly lst (-. 1.)
  in 
  fold_left (fun a b -> a +. b) 0. (map solve z)
let print_lst lst = 
  iter (printf "%.0f ") lst; 
  printf "\n"

(* let () = iter print_lst z *)

let () = printf "part 1: %.2f\n" p1
let () = printf "part 2: %.2f\n" p2

