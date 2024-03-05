(* 1. *)
let double x = 2*x
(*val double : int -> int = <fun>*)
let square x = x*x
(*val square : int -> int = <fun>*)
let twice f x = f (f x)
(*val twice : ('a -> 'a) -> 'a -> 'a = <fun>*)
let quad = twice double
(*val quad : int -> int = <fun>*)
let fourth = twice square
(*val fourth : int -> int = <fun>*)

(*2.*)
let ( $ ) f x = f x
(*val ( $ ) : ('a -> 'b) -> 'a -> 'b = <fun>*)

(*3.*)
let ( @@ ) f g x = x |> g |> f
(*val ( @@ ) : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b = <fun>*)
(*Example -
(String.length @@ string_of_int) 100;;
(String.length @@ string_of_float) 10.12;; *)


(*4. Generalize twice to a function repeat, such that repeat f n x applies f to x a total of n times.*)
let rec repeat f n x = 
  if n <= 0 then x
  else repeat f (n-1) (f x) ;;
(*val repeat : 'a -> int -> int -> int = <fun>*)

(*Example - *)
let addd x = x + 1;;
repeat addd 3 5;;  (* addd( addd( addd 5)) *)
(*- : int = 8*)


(*5. Use fold_left & fold_right to write a function product_left & product_right that computes the product of a list of floats.*)
let product_left lst = 
  List.fold_left (fun acc x -> acc *. x) 1.0 lst
(*val product_left : float list -> float = <fun>*)

let product_right lst =
  List.fold_right (fun x acc -> x *. acc) lst 1.0
(*val product_right : float list -> float = <fun>*)


(*6. Terse version of the 'product' exercise.*)
let product_left1 = List.fold_left ( *. ) 1.0 ;;
(*val product_left1 : float list -> float = <fun>*)

let product_right1 = ListLabels.fold_right ~f:( *. ) ~init:1.0 ;;
(*val product_right1 : float list -> float = <fun>*)

(*7. A function sum_cube_odd n that computes the sum of the cubes of all the odd numbers between 0 and n inclusive.*)
let sum_cube_odd n = 
  let odd_numbers = List.filter (fun x -> x mod 2 <> 0) (List.init (n + 1) (fun x -> x)) in
  let cubes = List.map (fun x -> x * x * x) odd_numbers in
  List.fold_left (+) 0 cubes
(*val sum_cube_odd : int -> int = <fun>*)
(* Simple version using recursion - *)
let rec sum_cube_odd1 n = 
  if n <= 0 then 0
  else if n mod 2 <> 0
    then n*n*n + sum_cube_odd1 (n - 1)
  else 
    sum_cube_odd1 (n - 1)
(*val sum_cube_odd1 : int -> int = <fun>*)


(*8. Rewrite the function sum_cube_odd to use the pipeline operator |>.*)
let sum_cube_odd2 n =
  List.init (n + 1) (fun x -> x)
  |> List.filter (fun x -> x mod 2 <> 0)
  |> List.map (fun x -> x * x * x)
  |> List.fold_left (+) 0
(*val sum_cube_odd2 : int -> int = <fun>*)


(*9. Consider writing a function exists: ('a -> bool) -> 'a list -> bool, 
such that exists p [a1; ...; an] returns whether at least one element of the list satisfies the predicate p. 
That is, it evaluates the same as (p a1) || (p a2) || ... || (p an). When applied to an empty list, it evaluates to false.
Write three solutions to this problem, as we did above:
• exists_rec, which must be a recursive function that does not use the List module,
• exists_fold, which uses either List.fold_left or List.fold_right, but not any other List module
functions nor the rec keyword, and
• exists_lib, which uses any combination of List module functions other than fold_left or
fold_right, and does not use the rec keyword.*)
let rec exists_rec p list = match list with
| [] -> false
| x :: rest -> p x || exists_rec p rest 
(*val exists_rec : ('a -> bool) -> 'a list -> bool = <fun>*)

let exists_fold p lst =
  List.fold_left (fun acc x -> acc || p x) false lst
(*val exists_fold : ('a -> bool) -> 'a list -> bool = <fun>*)

let exists_lib p lst = List.exists p lst
(*val exists_lib : ('a -> bool) -> 'a list -> bool = <fun>*)

(*Example - *)
let is_even x = x mod 2 == 0 ;;
exists_fold is_even [3;5;7;9];;
(*- : bool = false*)


(*10. A function which, given a list of numbers representing debits, deducts them from an account balance, 
and finally returns the remaining amount in the balance. Write three versions: fold_left, fold_right, 
and a direct recursive implementation.*)
let rec remaining_balance balance debits = 
  match debits with
  | [] -> balance
  | debit :: rest -> remaining_balance (balance - debit) rest
(*val remaining_balance : int -> int list -> int = <fun>*)

let remaining_balance1 balance debits = 
  List.fold_left (fun acc debit -> acc - debit) balance debits
(*val remaining_balance1 : int -> int list -> int = <fun>*)

let remaining_balance2 balance debits = 
  List.fold_right (fun debit acc -> acc - debit) debits balance;;
(*val remaining_balance2 : int -> int list -> int = <fun>*)

(*Example - *)
remaining_balance 100 [10;5;8;3];;
(*- : int = 74*)


(*11. *)
(* Uncurried version of List.nth: *)
let uncurried_nth (lst, n) = List.nth lst n
(*val uncurried_nth : 'a list * int -> 'a = <fun>*)

(* Uncurried version of List.append: *)
let uncurried_append (lst1, lst2) = List.append lst1 lst2
(*val uncurried_append : 'a list * 'a list -> 'a list = <fun>*)

(* Uncurried version of Char.compare: *)
let uncurried_char_compare (x, y) = Char.compare x y
(*val uncurried_char_compare : char * char -> int = <fun>*)

(* Uncurried version of Stdlib.max: *)
let uncurried_stdlib_max (x, y) = Stdlib.max x y
(*val uncurried_stdlib_max : 'a * 'a -> 'a = <fun>*)


(*12. Show how to replace any expression of the form List.map f (List.map g lst) 
with an equivalent expression that calls List.map only once.*)
(*Example -*)
let g x = x * x ;;
let f x = x + 1 ;;
(*val g : int -> int = <fun>
val f : int -> int = <fun>*)

let result lst = List.map f (List.map g lst);;
(*val result : int list -> int list = <fun>*)
(*Equivalent to - *) 
let result1 lst = List.map (fun x -> f (g x)) lst ;;
(*val result1 : int list -> int list = <fun>*)


(*13. Write functions that perform the following computations. 
Each function should use one of List.fold, List.map or List.filter. 
• Find those elements of a list of strings whose length is strictly greater than 3.
• Add 1.0 to every element of a list of floats.
• Given a list of strings strs and another string sep, produce the string that contains every element of strs
separated by sep. For example, given inputs ["hi";"bye"] and ",", produce "hi,bye", being sure not to
produce an extra comma either at the beginning or end of the result string.*)
let string_3 lst = List.filter (fun s -> String.length s > 3) lst
(*val string_3 : string list -> string list = <fun>*)

let add_1_float lst = List.map (fun a -> a +. 1.0) lst
(*val add_1_float : float list -> float list = <fun>*)

let string_concat strs sep = match strs with
  | [] -> ""
  | hd :: tl -> List.fold_left (fun acc s -> acc ^ sep ^ s) hd tl;;
(*val string_concat : string list -> string -> string = <fun>*)

(*Example - *)
string_3 ["apple";"egg";"dog";"ball"];;
(*- : string list = ["apple"; "ball"]*)
add_1_float [2.;4.;6.;8.];;
(*- : float list = [3.; 5.; 7.; 9.]*)
string_concat ["hi";"bye"] ",";;
(*- : string = "hi,bye"*)


(*14. Write a function keys: ('a * 'b) list -> 'a list that returns a list of the unique keys in an association list. 
Since they must be unique, no value should appear more than once in the output list. The order of values output does not matter.*)
let keys assoc_list = List.sort_uniq compare (List.map fst assoc_list)
(*val keys : ('a * 'b) list -> 'a list = <fun>*)

(*Alternative - *)
let keys1 assoc_list =
  List.fold_left (fun acc (key, _) ->
    if List.mem key acc then acc     (* Skip if the key is already in the accumulator *)
    else key :: acc) [] assoc_list     (* Add the key to the accumulator otherwise *)
  |> List.rev;;                  (* Reverse the result to maintain the original order of appearance *)
(*val keys : ('a * 'b) list -> 'a list = <fun>*)

(*Example - *)
keys [("apple", 1); ("banana", 2); ("orange", 3); ("apple", 4); ("grape", 5); ("lichi", 6)];;
keys1 [("apple", 1); ("banana", 2); ("orange", 3); ("apple", 4); ("grape", 5); ("lichi", 6)];;


(*14. Implement a function is_valid_matrix: int list list -> bool that returns whether the input matrix is valid or not.*)
let is_valid_matrix matrix = match matrix with
  | [] -> false
  | row :: rows ->
    let num_cols = List.length row in
    let valid_row row' = List.length row' = num_cols in
    List.for_all valid_row rows;;
(*val is_valid_matrix : 'a list list -> bool = <fun>*)


(*15. Implement a function add_row_vectors: int list -> int list -> int that does list for the element-wise addition of two row vectors.*)
let add_row_vectors vect1 vect2 = List.map2 (+) vect1 vect2
(*val add_row_vectors : int list -> int list -> int list = <fun>*)

(*Without using list function :-*)
let rec add_row_vectors1 v1 v2 = match (v1, v2) with
| ([], []) -> []
| (x1 :: rest1, x2 :: rest2) -> (x1 + x2) :: add_row_vectors1 rest1 rest2
| (_, _) -> invalid_arg "Vectors must have the same length" ;;
(*val add_row_vectors1 : int list -> int list -> int list = <fun>*)

(*Example - *)
add_row_vectors1 [1; 1; 1] [9; 8; 7];;


(*16. Implement a function add_matrices: int list -> int list -> int list for matrix addition.*)
let add_matrices vector1 vector2 = add_row_vectors vector1 vector2
(*val add_matrices : int list -> int list -> int list = <fun>*)


(*17. Implement a function multiply_matrices: int list list -> int list list -> int list list for matrix multiplication.*)
let transpose_matrix matrix =
  match matrix with
  | [] -> []
  | [] :: _ -> invalid_arg "Invalid Matrix!"
  | _ ->
    let num_rows = List.length matrix in
    let num_cols = List.length (List.hd matrix) in
    let transposed = Array.make_matrix num_cols num_rows 0 in
    for i = 0 to num_rows - 1 do
      for j = 0 to num_cols - 1 do
        transposed.(j).(i) <- List.nth (List.nth matrix i) j
      done
    done;
    Array.to_list (Array.map Array.to_list transposed)

let dot_product vector1 vector2 =
  List.fold_left2 (fun acc x1 x2 -> acc + (x1 * x2)) 0 vector1 vector2

let multiply_matrices matrix1 matrix2 =
  let transposed_matrix2 = transpose_matrix matrix2 in
  List.map (fun row1 -> List.map (fun col2 -> dot_product row1 col2) transposed_matrix2) matrix1
(*val multiply_matrices : int list list -> int list list -> int list list = <fun>*)

(*Example :- *)
multiply_matrices [[1; 2; 3]; [4; 5; 6]] [[7; 8]; [9; 10]; [11;5]];;
(*- : int list list = [[58; 43]; [139; 112]]*)
