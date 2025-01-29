(* Using this type:
type 'a sequence = Cons of 'a * (unit -> 'a sequence)
Define a value pow2 : int sequence whose elements are the powers of two: <1; 2; 4; 8; 16, ...>. *)
type 'a sequence = Cons of 'a * (unit -> 'a sequence);;

let pow2 = 
  let rec generate n = Cons (n, fun() -> generate (n * 2)) 
  in generate 1;;

(* Example Usage *)
let rec take n (Cons(x, rest)) = match n with
| 0 -> []
| _ -> x :: take (n - 1) (rest());;

take 10 pow2;;
(* - : int list = [1; 2; 4; 8; 16; 32; 64; 128; 256; 512] *)

(* Define the following sequences:
• the even naturals
• the lower-case alphabet on endless repeat: a, b, c, …, z, a, b, …
• unending pseudorandom coin flips (e.g., booleans or a variant with Heads and Tails constructors) *)

(* Even Naturals *)
let even_natural = 
  let rec generate n = Cons (n, fun() -> generate (n + 2))
  in generate 0;;
 
(* Example *)
take 10 even_natural;;
(* - : int list = [0; 2; 4; 6; 8; 10; 12; 14; 16; 18] *)


(* Lower-case Alphabet on Endless Repeat *)
let low_alphabet = 
  let rec generate i = 
    let letters = "abcdefghijklmnopqrstuvwxyz" in
    Cons (letters.[i mod 26], fun () -> generate (i + 1)) 
  in generate 0;;

(* Example *)
take 40 low_alphabet;;


(* Unending Pseudorandom Coin Flips *)
type coin = Heads | Tails

let coin_flip =
  let rec generate () =
    let flip = if Random.bool() then Heads else Tails in
    Cons (flip, fun () -> generate ()) 
  in
  generate ();;

take 7 coin_flip;;