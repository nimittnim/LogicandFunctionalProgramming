(*Types*)
    (1 : int);;
    ("hello" : string);;
    (3. : float);;
    ('c' : char);;
    (fun x -> x);; (*Unknown type, denoted by a' -> a'*)

(*Type Conversion*)
    int_of_string "2";; (*similarly others*)

(*Expressions*)
    4 + 5;;
    true && false;;
    (*if-else is also expression*)
        if 2 = 2 then "yes" else if 2 = 3 then "no" else "strange" (*value is yes*) (*else is always required for if*)

(*definitions*)
    let x = 2;; (*this is not object will come back later*)
    x + 3;;

(*functions*)
    (*Unnamed/Lambda*)
    (fun x -> x*x) (5);;

    (*Named*)
    let square x = x * x;;
    square 5;;

    (*Function types*)
        let add x y = x + y;; (*Type of add is int -> int -> int takes two ints returns one*)
        let add3 = add 3;; (*you can apply partially add 3 has type int -> int*)

    (*Recursive Function*)
        let rec fact n = if n = 1 then 1 else (n * fact (n-1));;

        (*Tail recurscion*)
            let rec fact n = if n = 1 then 1 else (n * fact (n-1));;

            (* With tail recursion*)
            let rec fact_aux n ax = if n = 1 then ax else fact_aux (n-1) (ax*n);; (*recursive call is at end nothing happens after call so you can use same stack for next call*)
            let rec fact_tr n = fact_aux n 1;;

(*Scoping*)
    (*Let Expression*)
        let x = 2 in ((let x = 3 in x) + x);; (*Output = 5*)
        (*So let is basically kinda substitues the var "in" next block*)
        (*Inner scope overshadows outer, and inner vars are not accesible in outer*)
    (*Definition is also let expression*)
        let x = 3;;  (*in*)
            let x = 4 in 
                x;;
        x;; (*output 3*)
        let x = 42;;
            let f x y = x + y;;
                f 0;; (*42*)
            let x = 22;;
                f 0;; (*42*)

    (*let expression is also a function sementically*)
        let x = 2 in x + 3;;
        (fun x -> 2) x + 3;;


(*Exercises*)
7 * (1+2+3);; (*type in int*)
("CS " ^ string_of_int 3110);; (*Concatanation of strings: string*)

42*10;;
3.14 /. 2.;;
4.2 ** 7.;;

42 = 42;;
"hi" = "hi";; (*checks value*)
"hi" == "hi";; (* checks if they are same*)

assert (2110 != 3110);;

if 2 > 3 then 42 else 7;;

let double x = 2*x;;
assert (double 7 = 14);;

let rms x y = ((x *. x +. y*.y) /. 2.) ** (0.5);;

(*slow*)
let rec fib n = if n = 0 then 0 else if n = 1 then 1 else (fib (n-1)) + (fib (n-2));;

(*fast*)
let rec fib_fast pp p n = if n = 1 then p else fib_fast (p) (pp + p) (n-1);;

let f x = if x then x else x (*x needs to bool. x is also returned*)
let g x y = if y then x else x
let h x y z = if x then y else z
let i x y z = if x then y else y

let divide ~numerator:arg1 ~denominator:arg2 = arg1/.arg2;;
(* divide ~denominator:2. ~numerator:4. |> prerr_float;; *)

let add x y = x + y;;
add 5 1;; (*int*)
add 5;; (*int -> int*)
(add 5) 1;; (*int*)
(* add (5 1);; Invalid *) 

let (+/.) x y = (x +. y)/. 2.;;
(* (+/.) 2. 2. |> print_float;; *)

