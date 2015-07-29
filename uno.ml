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
    | h::t -> if (prev_ele <> h || is_first) 
              then aux (h::compressed) h false t 
              else aux compressed h false t
  in List.rev (aux [] (List.hd list) true list);;

compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;

(* 9: Pack consecutive duplicates of list elements into sublists. (medium) *)

let pack list =
  let rec aux container consecutives prev_ele = function
    | [] -> consecutives::container 
    | h::t -> if (h = prev_ele) 
              then aux container (h::consecutives) h t 
              else aux (consecutives::container) (h::[]) h t
  in List.rev (aux [] [] (List.hd list) list);;

pack ["a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"];;

(* 10: Run-length encoding of a list. (easy) *)

let encode list =
  let rec aux counter container consecutives prev_ele = function
    | [] -> consecutives::container 
    | h::t -> if (h = prev_ele) 
              then aux (counter+1) container ((h,counter)::[]) h t 
              else aux 2 (consecutives::container) ((h,1)::[]) h t
  in List.rev (aux 1 [] [] (List.hd list) list);;

encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;

(* 11: Modified run-length encoding. (easy)
       Modify the result of the previous problem in such a way that if an element has no duplicates it is simply copied into the result list.
       Only elements with duplicates are transferred as (N E) lists.
       Since OCaml lists are homogeneous, one needs to define a type to hold both single elements and sub-lists. *)

type 'a rle =
    | One of 'a
    | Many of 'a * int;;

let encode list =
  let rec aux counter container consecutives prev_ele = function
    | [] -> (List.hd consecutives)::container 
    | h::t -> if (h = prev_ele) 
              then aux (counter+1) container ((Many (h,counter))::[]) h t 
              else aux 2 ((List.hd consecutives)::container) ((One h)::[]) h t
  in List.rev (aux 1 [] [] (List.hd list) list);;

encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
 
(* 12: Decode a run-length encoded list. (medium)
       Given a run-length code list generated as specified in the previous problem, construct its uncompressed version. *)

let decode list =
  let rec add_to_list ele list = function
    | 0 -> list
    | _ as count -> add_to_list ele (ele::list) (count-1) in
  let rec aux result = function
    | [] -> result
    | One h::t -> aux (h::result) t
    | Many (ele,n)::t -> aux (add_to_list ele result n) t
  in aux [] (List.rev list);; 

decode [Many ("a",4); One "b"; Many ("c",2); Many ("a",2); One "d"; Many ("e",4)];;
