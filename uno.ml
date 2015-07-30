(* https://ocaml.org/learn/tutorials/99problems.html *)

(* 1: Write a function last : 'a list -> 'a option that returns the last element of a list. (easy)  *)

let rec last = function
  | [] -> None
  | [x] -> Some x
  | h::t -> last t;;

last [ "a" ; "b" ; "c" ; "d" ];;

(* 8: Eliminate consecutive duplicates of list elements. (medium) *)

let compress list= 
  let rec aux compressed = function
    | [] -> compressed
    | [x] -> x::compressed
    | h::(h'::t) -> if (h <> h') 
              then aux (h::compressed) (h'::t) 
              else aux compressed (h'::t)
  in List.rev (aux [] list);;

compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;

(* 9: Pack consecutive duplicates of list elements into sublists. (medium) *)

let pack list =
  let rec aux container consecutives = function
    | [] -> consecutives::container 
    | [x] -> aux (container) (x::consecutives) []
    | h::(h'::t) -> if (h = h') 
              then aux container (h::consecutives) (h'::t) 
              else aux ((h::consecutives)::container) [] (h'::t)
  in List.rev (aux [] [] list);;

pack ["a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"];;

(* 10: Run-length encoding of a list. (easy) *)

let encode list =
  let rec aux counter container = function
    | [] -> container 
    | [x] -> aux 0 ((x,(counter+1))::container) [] 
    | h::(h'::t) -> if (h = h') 
              then aux (counter+1) container (h'::t) 
              else aux 0 ((h,(counter+1))::container) (h'::t)
  in List.rev (aux 0 [] list);;

encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;

(* 11: Modified run-length encoding. (easy)
       Modify the result of the previous problem in such a way that if an element has no duplicates it is simply copied into the result list.
       Only elements with duplicates are transferred as (N E) lists.
       Since OCaml lists are homogeneous, one needs to define a type to hold both single elements and sub-lists. *)

type 'a rle =
    | One of 'a
    | Many of 'a * int;;

let encode list =
  let get_type x = function
    | 1 -> One x
    | _ as counter -> Many (x, counter) in
  let rec aux counter container = function
    | [] -> container 
    | [x] -> aux 0 ((get_type x (counter+1))::container) [] 
    | h::(h'::t) -> if (h = h') 
              then aux (counter+1) container (h'::t) 
              else aux 0 ((get_type h (counter+1))::container) (h'::t)
  in List.rev (aux 0 [] list);;

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

(* 14: Duplicate the elements of a list. (easy) *)

let duplicate list = 
  let rec repeat ele list = function
    | 0 -> list
    | _ as count -> repeat ele (ele::list) (count-1) in
  let rec aux result = function
    | [] -> result
    | h::t -> aux (repeat h result 2) t  
  in aux [] (List.rev list);;

duplicate ["a";"b";"c";"c";"d"];;

(* 15: Replicate the elements of a list a given number of times. (medium)*)

let replicate list n = 
  let rec repeat ele list = function
    | 0 -> list
    | _ as count -> repeat ele (ele::list) (count-1) in
  let rec aux result = function
    | [] -> result
    | h::t -> aux (repeat h result n) t  
  in aux [] (List.rev list);;

replicate ["a";"b";"c"] 3;;

(* 16: Drop every N'th element from a list. (medium) *)

let drop list n = 
  let rec aux result count = function
    | [] -> result
    | h::t -> if (count = n) 
              then aux result 1 t 
              else aux (h::result) (count+1) t
  in List.rev(aux [] 1 list);;

drop ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3;;

(* 17: Split a list into two parts; the length of the first part is given. (easy)

If the length of the first part is longer than the entire list, then the first part is the list and the second part is empty. *)

let split list first_part_length = 
  let rec aux first_part_length first_part second_part = function
    | [] -> (first_part,second_part)
    | (h::t) -> if (first_part_length > 0)
                then aux (first_part_length - 1) (h::first_part) second_part t
                else aux 0 first_part (h::second_part) t
  in aux first_part_length [] [] (List.rev(list));;

split ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3;;

(* 18: Extract a slice from a list. (medium)

Given two indices, i and k, the slice is the list containing the elements between the i'th and k'th element of the original list (both limits included). Start counting the elements with 0 (this is the way the List module numbers elements). *)

let slice list x y =
  let shift_list = function
    | [] -> []
    | (h::t) -> t in
  let rec aux x y result list = match (x,y) with
    | (_,-1) -> result 
    | (0, _) -> aux 0 (y-1) ((List.hd list)::result) (shift_list list)
    |  _     -> aux (x-1) (y-1) [] (shift_list list)
  in List.rev(aux x y [] list);;

slice ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 2 6;;
