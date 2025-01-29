(* Exercise problems - " Modular Programming " *)
(* ======================================================================================================================= *)

(* Here is a module type for complex numbers, which have a real and imaginary component: *)
(* module type ComplexSig = sig
  val zero : float * float
  val add : float * float -> float * float -> float * float
end *)
(*Improve that code by adding type t = float * float. Show how the signature can be written more tersely
because of the type synonym. *)
module type ComplexSig = sig
  type t = float * float
  val zero : t * t
  val add : t * t -> t * t -> t * t
end;;


(* Write a module that implements the Fraction module type below:
module type Fraction = sig
  (* A fraction is a rational number p/q, where q != 0.*)
  type t
  (* * [make n d] is n/d. Requires d != 0. *)
  val make : int -> int -> t
  val numerator : t -> int
  val denominator : t -> int
  val to_string : t -> string
  val to_float : t -> float
  val add : t -> t -> t
  val mul : t -> t -> t
end *)
module type Fraction = sig
  type t
  val make : int -> int -> t
  val numerator : t -> int
  val denominator : t -> int
  val to_string : t -> string
  val to_float : t -> float
  val add : t -> t -> t
  val mul : t -> t -> t
end;;

module FractionImpl : Fraction = struct
  type t = int * int  (* Representing the fraction as a tuple (numerator, denominator) *)

  let make n d = 
    if d = 0 then invalid_arg "Denominator cannot be zero."
    else (n, d)

  let numerator (n, _) = n

  let denominator (_, d) = d

  let to_string (n, d) = 
    if d = 1 then string_of_int n
    else Printf.sprintf "%d/%d" n d

  let to_float (n, d) = 
    float_of_int n /. float_of_int d

  let add (n1, d1) (n2, d2) =
    let numerator = n1 * d2 + n2 * d1 in
    let denominator = d1 * d2 in
    (numerator, denominator)

  let mul (n1, d1) (n2, d2) =
    let numerator = n1 * n2 in
    let denominator = d1 * d2 in
    (numerator, denominator)
end;;

(* module CharMap = Map.Make(Char);;
Using the CharMap you just made, create a map that contains the following bindings:
• 'A' maps to "Alpha"
• 'E' maps to "Echo"
• 'S' maps to "Sierra"
• 'V' maps to "Victor"
Use CharMap.find to find the binding for 'E'.
Now remove the binding for 'A'. Use CharMap.mem to find whether 'A' is still bound. *)
module CharMap = Map.Make(Char);;

let char_map = CharMap.(empty |> add 'A' "Alpha" |> add 'E' "Echa" |> add 'S' "Sierra" |> add 'V' "Victor");;
(* val char_map : string CharMap.t = <abstr> *)
CharMap.bindings char_map;;

CharMap.find 'E' char_map;;

let new_char = CharMap.remove 'A' char_map;;

CharMap.mem 'A' new_char;;

CharMap.bindings new_char;;


(* Here is a type for dates:
type date = {month : int; day : int}
For example, March 31st would be represented as {month = 3; day = 31}. Our goal in the next few exercises is
to implement a map whose keys have type date.
Obviously it’s possible to represent invalid dates with type date—for example, { month=6; day=50 } would be
June 50th, which is not a real date. The behavior of your code in the exercises below is unspecified for invalid dates.
To create a map over dates, we need a module that we can pass as input to Map.Make. That module will need to match
the Map.OrderedType signature. *)
type date = { month : int; day : int };;

module Date = struct
  type t = date
  let compare d1 d2 = compare d1.day d2.day
end;;


(* Use the Map.Make functor with your Date module to create a DateMap module. Then define a calendar type as
follows:
type calendar = string DateMap.t
The idea is that calendar maps a date to the name of an event occurring on that date.
Using the functions in the DateMap module, create a calendar with a few entries in it, such as birthdays or anniversaries. *)

module DateMap = Map.Make (Date);;

type calender = string DateMap.t;;

let date1 = {month = 2; day = 9};;
let date2 = {month = 1; day = 26};;
let date3 = {month = 8; day = 15};;

let my_calender = DateMap.(empty |> add date1 "Birthday" |> add date2 "Republic Day" |> add date3 "Independence Day");;

DateMap.bindings my_calender;;


(* Write a function print_calendar : calendar -> unit that prints each entry in a calendar in a format
similar to the inspiring examples in the previous exercise. Hint: use DateMap.iter, which is documented in the Map.
S signature. *)

let print_calendar calendar =
  DateMap.iter (fun date event ->
    let month_name = 
      match date.month with
      | 1 -> "January"
      | 2 -> "February"
      | 3 -> "March"
      | 4 -> "April"
      | 5 -> "May"
      | 6 -> "June"
      | 7 -> "July"
      | 8 -> "August"
      | 9 -> "September"
      | 10 -> "October"
      | 11 -> "November"
      | 12 -> "December"
      | _ -> "Unknown"
    in
    Printf.printf "%s %d: %s\n" month_name date.day event
  ) calendar;;
  
print_calendar my_calender;;


(* Write a function is_for : string CharMap.t -> string CharMap.t that given an input map with
bindings from 𝑘1 to 𝑣1, …, 𝑘𝑛 to 𝑣𝑛, produces an output map with the same keys, but where each key 𝑘𝑖 is now bound to
the string “𝑘𝑖 is for 𝑣𝑖”. For example, if m maps 'a' to "apple", then is_for m would map 'a' to "a is for apple". 
Hint: there is a one-line solution that uses a function from the Map.S signature. To convert a character to a
string, you could use String.make. An even fancier way would be to use Printf.sprintf. *)
module CharMap = Map.Make(Char);;

let is_for (m : string CharMap.t) : string CharMap.t =
  CharMap.map (fun v -> Printf.sprintf "%s is for %s" (String.make 1 v.[0]) v) m;;

