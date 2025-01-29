(* Exercise problems - " Mutability " *)
(* ======================================================================================================================= *)


(* Define an OCaml record type to represent student names and GPAs. It should be possible to mutate the value of a student’s
GPA. Write an expression defining a student with name "Alice" and GPA 3.7. Then write an expression to mutate
Alice’s GPA to 4.0. *)
type student = {
  name : string;
  mutable gpa : float;
};;

let alice = { name = "Alice"; gpa = 3.7 };;
alice.gpa <- 4.0;;

(* Define a reference to a function as follows:
let inc = ref (fun x -> x + 1)
Write code that uses inc to produce the value 3110. *)
let inc = ref (fun x -> x + 1);;
let result = (List.fold_left (fun acc _ -> !inc acc) 3109 (List.init 1 (fun _ -> ())));;  (* Apply once *)
(* OR *)
let apply_inc n =
  let rec loop acc times =
    if times = 0 then acc
    else loop (!inc acc) (times - 1)
  in
  loop n 1  (* Apply inc once *)

let result1 = apply_inc 3109;;


(* The Array module contains two functions for creating an array: make and init. make creates an array and fills it
with a default value, while init creates an array and uses a provided function to fill it in. The library also contains a
function make_matrix for creating a two-dimensional array, but it does not contain an analogous init_matrix to
create a matrix using a function for initialization.
Write a function init_matrix : int -> int -> (int -> int -> 'a) -> 'a array array such
that init_matrix n o f creates and returns an n by o matrix m with m.(i).(j) = f i j for all i and j in
bounds.
See the documentation for make_matrix for more information on the representation of matrices as arrays. *)
let init_matrix (n : int) (o : int) (f : int -> int -> 'a) : 'a array array =
  Array.init n (fun i -> 
    Array.init o (fun j -> f i j)
  );;
let matrix = init_matrix 3 3 (fun i j -> i + j);;