(* https://ocaml.org/learn/tutorials/99problems.html *)

(* 1: Write a function last : 'a list -> 'a option that returns the last element of a list. (easy)  *)

let rec last = function
  | [] -> None
  | [x] -> Some x
  | h::t -> last t;;

last [ "a" ; "b" ; "c" ; "d" ];;

(* 8: Eliminate consecutive duplicates of list elements. (medium) *)

let compress list= 
  let rec aux compressed prev_ele is_first = function
    | [] -> compressed
    | h::t -> if (prev_ele <> h || is_first) then aux (h::compressed) h false t else aux compressed h false t
  in List.rev (aux [] (List.hd list) true list);;

compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;

(* 9: Pack consecutive duplicates of list elements into sublists. (medium) *)

let pack list =
  let rec aux container consecutives prev_ele = function
    | [] -> consecutives::container 
    | h::t -> if (h = prev_ele) then aux container (h::consecutives) h t else aux (consecutives::container) (h::[]) h t
  in List.rev (aux [] [] (List.hd list) list);;

pack ["a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"];;
