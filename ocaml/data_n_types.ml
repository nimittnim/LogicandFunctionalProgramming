(*List*)
  1 :: 2 :: [];;
  [1, 2];;
  (*list is already a value*)

(*Pattern Matching*)
  let rec sum lst = match lst with 
    | [] -> 0
    | h :: t -> h + sum t;;

  (* sum (1 :: 2 :: []) |> print_int;; *)

(*Variants*)
  type day = SUN | MON | SAT
  let d = SUN;; (*SUN, MON, are constructors, upper cased, already a value*)

  let int_of_day d = match d with
    | SUN -> 1
    | MON -> 2
    | SAT -> 3

  (*Scope*)
  type t1 = D | C
  type t2 = D | E

  let x = D;; (*t2 overshadows t1, add extra character to distinguish*) 

(*Records*)
  type ptype = WATER | NORMAL | FIRE
  type pmon = { name : string; hp : int; ptype: ptype; };;
  let c = {name = "Charmander"; hp = 39; ptype = FIRE};; (*record expression*)
  (*Accessing*)
  c.name;; (*c.f then f shoud be an identifier of a field name not expression*)
  (*Pattern Matching*)
  match c with | {name = n; hp = h; ptype = p} -> h;;
  match c with | {name; hp; ptype;} -> hp;;


(*Tuples*)
  (1, 2, 3);;
  ("hello", 1);;
  (*Pattern Matching*)
  match (1, 2, 3) with | (x, y, z) -> x + y + z;;

(*Advanced Pattern Matching*)
  match d with | SUN | MON | SAT -> 4;;
  (1 : int);; (*patter with annotation*)
  (*c is for constant, ch for char*)
  (*p when e*)
  (*let expression is match*)

(*Example*)
  (* Pokemon types *)
  type ptype = TNormal | TFire | TWater

  (* A record to represent Pokemon *)
  type mon = { name : string; hp : int; ptype : ptype }

  (* OK *)
  let get_hp m = match m with { name = n; hp = h; ptype = t } -> h

  (* better *)
  let get_hp m = match m with { name = _; hp = h; ptype = _ } -> h

  (* better *)
  let get_hp m = match m with { name; hp; ptype } -> hp

  (* better *)
  let get_hp m = match m with { hp; _ } -> hp

  (* best *)
  let get_hp m = m.hp

  let fst (x, _) = x

  let snd (_, y) = y

  (* OK *)
  let thrd t = match t with x, y, z -> z

  (* good *)
  let thrd t =
    let x, y, z = t in
    z

  (* better *)
  let thrd t =
    let _, _, z = t in
    z

  (* best *)
  let thrd (_, _, z) = z

(*Type Synonyms*)
  type point = float*float

  let get_x = fun (x, _) -> x;;

  let p1 : point = (1., 2.);;
  let p2 : float*float = (2.,3.);;

  get_x (p1);; get_x (p2);;

(*Options*)
    (*like a box: either none or some*)
    let rec list_max = function
      | [] -> None
      | h :: t -> begin
        match list_max t with
          | None -> Some h
          | Some g -> Some (max h g)
        end

(*Variants are algebraic data types*)
  type point = float * float
  type shape =
    | Point of point
    | Circle of point * float (* Point, Circle a-also-ka tags *)
    | Rect of point * point 
  let center = function
    | Point p -> p
    | Circle (p, _) -> p
    | Rect ((x1, y1), (x2, y2)) -> ((x2 +. x1) /. 2.0, (y2 +. y1) /. 2.0)

  type string_or_int =
  | String of string
  | Int of int
    
  (*Avoid catch all cases in match*)

(*Exercises*)
  (*List Expressions*)
  let x = [1; 2; 3; 4; 5];;
  let x = 1 :: 2 :: 3 :: 4 :: 5 :: [];;
  let x = [1] @ [2; 3; 4] @ [5];;

  let rec product lst = match lst with 
  | [] -> 1
  | x :: lst' -> x * (product lst');;
  
  (* product (1 :: 2 :: 3 :: 4 :: []) |> print_int;; *)

  let rec concat lst = match lst with
  | [] -> ""
  | s :: lst' -> s ^ (concat lst');;

  (* concat ("nimitt" :: "is" :: "nimitt" :: []) |> print_string;; *)

  let rec check1 lst = match lst with
  | "bigred" :: lst' -> true
  | _ -> false

  let rec check2 lst = match lst with
  | _ :: _ :: [] -> true
  | _ :: _ :: _ :: _ :: [] -> true
  | _ -> false

  let rec check3 lst = match lst with
  | x1 :: x2 :: _ -> x1 = x2
  | _ -> false

  let fifth lst =
    if List.length lst >= 5 then
      List.nth lst 4
    else
      0
  ;;

  let descending_sort (lst : int list) =
    List.rev (List.sort Stdlib.compare lst)
  ;;

  let last (lst : int list) = List.nth (List.rev lst) (1);;
  
  let any_zeros lst = List.mem 0 lst;;
  if any_zeros (1 :: []) then print_string ("true");;

  let rec print_list lst = match lst with
    | [] -> ()
    | x :: lst' -> Printf.printf "%d\n" x; print_list lst';;

  let rec take n lst = 
    if (n <= 0) then [] else
    match lst with 
    | [] -> []
    | x :: lst' -> x :: (take (n-1) lst');;

  (* print_list (take (3) (1 :: 2 :: []));; *)
  
  let rec drop n lst = 
    if n <= 0 then lst else
      match lst with 
      | [] -> []
      | _ :: lst' -> drop (n-1) lst';;
  
  (* print_list (drop (1) (1 :: 2 :: 3 :: []));; *)

  let rec take_acc cur n lst = 
    if n <= 0 then cur else
      match lst with
      | [] -> cur
      | x :: lst' -> take_acc (x :: cur) (n-1) lst';;
  let take_tr n lst = take_acc [] n lst;;

  (* print_list (take (3) (1 :: 2 :: 3 :: 4 :: []));; *)

  let rec is_mon_dec lst = match lst with
   | [] | [_] -> true
   | x1 :: (x2 :: lst2 as lst1) -> x1 >= x2 && is_mon_dec lst1;;  
  let rec is_unimodal lst = match lst with
   | [] | [_] -> true
   | x1 :: (x2 :: lst2 as lst1) -> if x1 <= x2 then is_unimodal lst1 else is_mon_dec lst1;;

  let rec powerset (lst : int list) = match lst with
   | [] -> []
   | x :: lst1 as lst0 -> lst0 :: lst1 :: [];;

   (* print_list (1 :: 2 :: 3 :: []);; *)

   let print_int_list_iter lst =
    List.iter (fun x -> Printf.printf "%d\n" x) lst;;
  
  (* print_int_list_iter (1 :: 2 :: 3 :: []);; *)

  (*types*)
  type student = {first_name : string; last_name : string; gpa : float;};;

  let std = {first_name="Nimitt"; last_name="Nimitt"; gpa=9.13};;

  let get_name std = match std with 
    | {first_name; last_name; gpa} -> first_name, last_name;;
  
  let create_student first_name last_name gpa = {first_name = first_name; last_name = last_name; gpa = gpa};;
  
  type poketype = Normal | Fire | Water;;
  type pokemon = {name : string; hp : int; ptype: poketype};;

  let charizard = {name="charizard"; hp=78; ptype=Fire};;

  let squirtle = {name="squirtle"; hp = 44; ptype=Water};;

  let safe_hd lst = match lst with
   | [] -> None
   | x :: _ -> Some x;;

  let safe_tl lst = match lst with
   | [] -> None
   | _ :: lst1 -> Some lst1;;

  let rec max_hp lst = match lst with 
   | [] -> None
   | x1 :: lst1 -> match max_hp lst1 with
    | None -> Some x1
    | Some x2 -> Some (if x1.hp <= x2.hp then x2 else x1);;

  type date = int * int * int;;
  let is_before (date1 : date) (date2 : date) = 
    let (y1, m1, d1) = date1 in
    let (y2, m2, d2) = date2 in
    y1 < y2 || (y1 = y2 && m1 < m2) || (y1 = y2 && m1 = m2 && d1 < d2)


  