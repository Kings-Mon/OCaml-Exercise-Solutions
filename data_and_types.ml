(* Exercise problems - " Data And Types " *)
(* ======================================================================================================================= *)

(*The product of all the elements in a list.*)
let rec product = function
| [] -> 1
| h :: t -> h * product t;;
(*val product : int list -> int = <fun>*)


(*Concatenates all the strings in a list.*)
let rec concatinate_string = function
| [] -> ""
| h :: t -> h ^ concatinate_string t;;
(*val concatinate_string : string list -> string = <fun>*)
(* To concatenate a list of lists of integers into a single list of integers --
let concatinate_integer lst = List.concat lst;;*)


(* Using pattern matching, write three functions, that checks -
• the list’s first element is "bigred"
• the list has exactly two or four elements; do not use the length function
• the first two elements of the list are equal*)
let first_element = function
| "bigred" :: _ -> true
| _ -> false;;
(*val first_element : string list -> bool = <fun>*)

let two_or_four_elements = function
| _ :: _ :: [] | _ :: _ :: _ :: _ :: [] -> true
| _ -> false;;
(*val two_or_four_elements : 'a list -> bool = <fun>*)

let first_two_equal = function
| x :: y :: _ when x = y -> true
| _ -> false;;
(*val first_two_equal : 'a list -> bool = <fun>*)


(*A function that returns the fifth element of that list*)
let fifth_element_check lst = 
  if List.length lst >= 5 
    then List.nth lst 4
  else 0;;
(*val fifth_element_check : int list -> int = <fun>*)


(*A function that returns the list sorted in descending order*)
let decending_sort lst =
  List.sort (fun x y -> Stdlib.compare y x) lst;;
(*val decending_sort : 'a list -> 'a list = <fun>*)


(*A function that returns the last element of a list.*)
let last_element_return lst = List.hd (List.rev lst);;
(*val last_element : 'a list -> 'a = <fun>*)

(*Alternated version, which takes the list length :- *)
let last_element_return_1 n lst =
  if List.length lst = n 
    then List.nth lst (n - 1)
  else 0;;
(*val last_element_return_1 : int -> int list -> int = <fun>*)


(*A function that returns true if and only if the input list contains at least one 0.*)
let any_zeroes lst = List.exists (fun x -> x = 0) lst;;
(*val any_zeroes : int list -> bool = <fun>*)

(* Alternatively, *)
let any_zeroes_1 lst = List.mem 0 lst;;
(*val any_zeroes_1 : int list -> bool = <fun>*)


(*A function that returns the first n elements of lst.*)
let rec take n lst = match n, lst with
| 0, _ -> []
| _, [] -> []
| n, h :: t -> h :: take (n-1) t;;
(*val take : int -> 'a list -> 'a list = <fun>*)


(*A function that returns all but the first n elements of lst.*)
let rec drop n lst = match n, lst with
| 0, _ -> lst
| _, [] -> []
| n, _ :: t -> drop (n-1) t;;
(*val drop : int -> 'a list -> 'a list = <fun>*)


(*Tail Recursive Versions of take and drop :- *)

let take n lst = 
  let rec take_tail n acc lst = 
    match n, lst with
    | 0 , _ -> List.rev acc
    | _ , [] -> List.rev acc
    | n , h :: t -> take_tail (n-1) (h :: acc) t
  in
  take_tail n [] lst;;
(*val take : int -> 'a list -> 'a list = <fun>*)

let drop n lst = 
  let rec drop_tail n lst = 
    match n, lst with
    | 0 , _ -> lst
    | _ , [] -> []
    | n , _ :: t -> drop_tail (n-1) t
  in
  drop_tail n lst;;
(*val drop : int -> 'a list -> 'a list = <fun>*)

(*List Creation :- *)
let create_lst n = List.init n (fun x -> x);;
(*val create_lst : int -> int list = <fun>*)

let lst_1000 = create_lst 1000;;
(*val lst_1000 : int list =
  [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19;
   20; 21; 22; 23; 24; 25; 26; 27; 28; 29; 30; 31; 32; 33; 34; 35; 36; 37;
   38; 39; 40; 41; 42; 43; 44; 45; 46; 47; 48; 49; 50; 51; 52; 53; 54; 55;
   56; 57; 58; 59; 60; 61; 62; 63; 64; 65; 66; 67; 68; 69; 70; 71; 72; 73;
   74; 75; 76; 77; 78; 79; 80; 81; 82; 83; 84; 85; 86; 87; 88; 89; 90; 91;
   92; 93; 94; 95; 96; 97; 98; 99; 100; 101; 102; 103; 104; 105; 106; 107;
   108; 109; 110; 111; 112; 113; 114; 115; 116; 117; 118; 119; 120; 121;
   122; 123; 124; 125; 126; 127; 128; 129; 130; 131; 132; 133; 134; 135;
   136; 137; 138; 139; 140; 141; 142; 143; 144; 145; 146; 147; 148; 149;
   150; 151; 152; 153; 154; 155; 156; 157; 158; 159; 160; 161; 162; 163;
   164; 165; 166; 167; 168; 169; 170; 171; 172; 173; 174; 175; 176; 177;
   178; 179; 180; 181; 182; 183; 184; 185; 186; 187; 188; 189; 190; 191;
   192; 193; 194; 195; 196; 197; 198; 199; 200; 201; 202; 203; 204; 205;
   206; 207; 208; 209; 210; 211; 212; 213; 214; 215; 216; 217; 218; 219;
   220; 221; 222; 223; 224; 225; 226; 227; 228; 229; 230; 231; 232; 233;
   234; 235; 236; 237; 238; 239; 240; 241; 242; 243; 244; 245; 246; 247;
   248; 249; 250; 251; 252; 253; 254; 255; 256; 257; 258; 259; 260; 261;
   262; 263; 264; 265; 266; 267; 268; 269; 270; 271; 272; 273; 274; 275;
   276; 277; 278; 279; 280; 281; 282; 283; 284; 285; 286; 287; 288; 289;
   290; 291; 292; 293; 294; 295; 296; 297; 298; ...]*)

drop 900 lst_1000;;
(*- : int list =
[900; 901; 902; 903; 904; 905; 906; 907; 908; 909; 910; 911; 912; 913; 914;
 915; 916; 917; 918; 919; 920; 921; 922; 923; 924; 925; 926; 927; 928; 929;
 930; 931; 932; 933; 934; 935; 936; 937; 938; 939; 940; 941; 942; 943; 944;
 945; 946; 947; 948; 949; 950; 951; 952; 953; 954; 955; 956; 957; 958; 959;
 960; 961; 962; 963; 964; 965; 966; 967; 968; 969; 970; 971; 972; 973; 974;
 975; 976; 977; 978; 979; 980; 981; 982; 983; 984; 985; 986; 987; 988; 989;
 990; 991; 992; 993; 994; 995; 996; 997; 998; 999]*)

take 200 lst_1000;;
(*- : int list =
[0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20;
 21; 22; 23; 24; 25; 26; 27; 28; 29; 30; 31; 32; 33; 34; 35; 36; 37; 38;
 39; 40; 41; 42; 43; 44; 45; 46; 47; 48; 49; 50; 51; 52; 53; 54; 55; 56;
 57; 58; 59; 60; 61; 62; 63; 64; 65; 66; 67; 68; 69; 70; 71; 72; 73; 74;
 75; 76; 77; 78; 79; 80; 81; 82; 83; 84; 85; 86; 87; 88; 89; 90; 91; 92;
 93; 94; 95; 96; 97; 98; 99; 100; 101; 102; 103; 104; 105; 106; 107; 108;
 109; 110; 111; 112; 113; 114; 115; 116; 117; 118; 119; 120; 121; 122; 123;
 124; 125; 126; 127; 128; 129; 130; 131; 132; 133; 134; 135; 136; 137; 138;
 139; 140; 141; 142; 143; 144; 145; 146; 147; 148; 149; 150; 151; 152; 153;
 154; 155; 156; 157; 158; 159; 160; 161; 162; 163; 164; 165; 166; 167; 168;
 169; 170; 171; 172; 173; 174; 175; 176; 177; 178; 179; 180; 181; 182; 183;
 184; 185; 186; 187; 188; 189; 190; 191; 192; 193; 194; 195; 196; 197; 198;
 199]*)

(* #################################################################################### *)


(*A unimodal list is a list that monotonically increases to some maximum value then monotonically decreases after that value. 
Either or both segments (increasing or decreasing) may be empty. 
A constant list is unimodal, as is the empty list.*)
(* // A function that takes an integer list and returns whether that list is unimodal. // *)
let rec is_unimodal lst = match lst with
| [] | [_] -> true (*Empty list and single-element list are unimodal*)
| x :: y :: t -> 
  if x <= y then
    is_unimodal (y :: t) (*Increasing segment, check the rest of the list*)
  else
    (*Decreasing segment, find the maximum and check the rest of the list*)
    let rec find_max_and_check rest max_val = 
      match rest with
      | [] -> is_unimodal (List.rev_append [max_val] t)
      | h :: t ->
        if h > max_val then
          find_max_and_check t h 
        else
          false
    in
    find_max_and_check t x;;
(*val is_unimodal : 'a list -> bool = <fun>*)


(*A function that takes a set S represented as a list and returns the set of all subsets of S.*)
let rec powerset s = match s with
| [] -> [[]]
| x :: rest -> 
  let rest_set = powerset rest in
  rest_set @ List.map (fun subset -> x :: subset) rest_set;;
(*val powerset : 'a list -> 'a list list = <fun>*)


(*A function that prints its input list, one number per line.*)
let rec print_int_list = function
| [] -> ()
| hd :: tl -> 
  print_int hd;
  print_newline ();
  print_int_list tl;;
(*val print_int_list : int list -> unit = <fun>*)

(*Without using Recurssion :- *)
let print_int_list' lst = List.iter (fun x -> print_int x; print_newline()) lst;;
(*val print_int_list' : int list -> unit = <fun>*)


type student = {first_name : string; last_name : string; gpa : float};;
(* Give OCaml expressions that have the following types:
• student
• student -> string * string (a function that extracts the student’s name)
• string -> string -> float -> student (a function that creates a student record) *)

let example_student : student = {first_name = "Kingshuk"; last_name = "Mondal"; gpa = 8.1}
(*val example_student : student = {first_name = "Kingshuk"; last_name = "Mondal"; gpa = 8.1}*)

let extract_name : student -> string * string = 
  fun s -> (s.first_name, s.last_name);;
(*val extract_name : student -> string * string = <fun>*)

let create_student : string -> string -> float -> student = 
  fun first_name last_name gpa -> {first_name = first_name; last_name = last_name; gpa = gpa};;
(*val create_student : string -> string -> float -> student = <fun>*)
(*create_student "Satyajit" "Roy" 9.0;;
- : student = {first_name = "Satyajit"; last_name = "Roy"; gpa = 9.}*)


type poketype = Normal | Fire | Water;;
(*• Define the type pokemon to be a record with fields name (a string), hp (an integer), and ptype (a poketype).
  • Create a record named charizard of type pokemon that represents a Pokémon with 78 HP and Fire type.
  • Create a record named squirtle of type pokemon that represents a Pokémon with 44 HP and Water type. *)

type pokemon = {name : string; hp : int; ptype : poketype};;
(*type pokemon = { name : string; hp : int; ptype : poketype; }*)

let charizard : pokemon = {name = "Charizard"; hp = 78; ptype = Fire};;
(*val charizard : pokemon = {name = "Charizard"; hp = 78; ptype = Fire}*)

let squirtle : pokemon = {name = "Squirtle"; hp = 44; ptype = Water};;
(*val squirtle : pokemon = {name = "Squirtle"; hp = 44; ptype = Water}*)

(*A function max_hp : pokemon list -> pokemon option that, given a list of pokemon and finds the Pokémon with the highest HP.*)
let max_hp : pokemon list -> pokemon option = 
 fun pokemons ->
  match pokemons with
  | [] -> None
  | first :: rest -> 
    let max_hp_pokemon = 
      List.fold_left
        (fun acc_pokemon current_pokemon ->
          if current_pokemon.hp > acc_pokemon.hp then current_pokemon
          else acc_pokemon)
        first 
        rest
    in
    Some max_hp_pokemon;;
(* Example Pokemon list : *)
let pokemon_list = [
  {name = "Charizard"; hp = 78; ptype = Fire};
  {name = "Squirtle"; hp = 44; ptype = Water};
  {name = "Blastoise"; hp = 79; ptype = Water};
  {name = "Pikachu"; hp = 35; ptype = Normal}; ];;
(*val pokemon_list : pokemon list =
  [{name = "Charizard"; hp = 78; ptype = Fire};
   {name = "Squirtle"; hp = 44; ptype = Water};
   {name = "Blastoise"; hp = 79; ptype = Water};
   {name = "Pikachu"; hp = 35; ptype = Normal}]
*)

max_hp pokemon_list;;
(*- : pokemon option = Some {name = "Blastoise"; hp = 79; ptype = Water} *)


(*A function that returns Some x if the head of the input list is x.*)
let safe_hd = function
| [] -> None
| hd :: _ -> Some hd;;
(*val safe_hd : 'a list -> 'a option = <fun>*)

(*A function that returns the tail of the list.*)
let safe_tl = function
| [] -> None
| _ :: tl -> Some tl;;
(*val safe_tl : 'a list -> 'a list option = <fun>*)


(*A function that takes two dates as input and evaluates to true or false. 
It evaluates to true if the first argument is a date that comes before the second argument. 
(If the two dates are the same, the result is false.)*)
let is_before (year1, month1, day1) (year2, month2, day2) = 
  if year1 < year2 then
    true
  else if year1 > year2 then
    false
  else if month1 < month2 then
    true
  else if month1 > month2 then
    false
  else
    day1 < day2;;
(*val is_before : 'a * 'b * 'c -> 'a * 'b * 'c -> bool = <fun>*)

(*Example :- *)
is_before (2018, 5, 16) (2018, 5, 20);;
(*- : bool = true*)


(*A function evaluates to None if the input list is empty, and to Some d if date d is the earliest date in the list.*)
let earliest dates = match dates with
| [] -> None
| first_date :: rest -> 
  let rec find_earliest current_earliest remaining_dates = 
    match remaining_dates with 
    | [] -> current_earliest
    | date :: rest_dates ->
      if is_before date current_earliest then
        find_earliest date rest_dates
      else
        find_earliest current_earliest rest_dates
  in
  Some (find_earliest first_date rest);;
(*val earliest : ('a * 'b * 'c) list -> ('a * 'b * 'c) option = <fun>*)

(* Example :- *)
earliest [(2003, 2, 1); (2003, 1, 15); (2003, 3, 10); (2003, 10, 29)];;
(*- : (int * int * int) option = Some (2003, 1, 15)*)


(* Insert function to add key-value pair to association list *)
let insert k v lst = (k, v) :: lst;;
(*val insert : 'a -> 'b -> ('a * 'b) list -> ('a * 'b) list = <fun>*)

insert 1 "one" (insert 2 "two" (insert 3 "three" (insert 4 "four"[])));;
(*- : (int * string) list = [(4, "four"); (3, "three"); (2, "two"); (1, "one")]*)
[] |> insert 1 "one" |> insert 2 "two" |> insert 3 "three" |> insert 4 "four";;
(*- : (int * string) list = [(4, "four"); (3, "three"); (2, "two"); (1, "one")]*)


(* Lookup function to retrieve the value associated with a given key *)
let rec lookup k = function
| [] -> None
| (k', v) :: t ->
  if k = k' then
    Some v
  else
    lookup k t;;
(*val lookup : 'a -> ('a * 'b) list -> 'b option = <fun>*)

(*Example - *)
(*let list123 = insert 1 "one" (insert 2 "two" (insert 3 "three" (insert 4 "four"[])));;
lookup 4 list123;; *)
(*- : string option = Some "four"*)


(*Define a variant type suit that represents the four suits, ♣ ♦ ♥ ♠, in a standard 52-card deck. 
All the constructors of your type should be constant.*)
type cards = Clubs | Diamonds | Hearts | Spades;;

(*Define a type rank that represents the possible ranks of a card: 2, 3, …, 10, Jack, Queen, King, or Ace. 
There are many possible solutions; you are free to choose whatever works for you. 
One is to make rank be a synonym of int, and to assume that Jack=11, Queen=12, King=13, and Ace=1 or 14. Another is to use variants.*)
type rank = int;; (*Synonym of int*)
(* Example usage -
let card_rank : rank = 7 *)
type rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace;; (*Using Variants*)
(* Example usage - 
let card_rank : rank = Jack *)

(*Define a type card that represents the suit and rank of a single card. Make it a record with two fields.*)
type card = { suit : cards; rank : rank };;

(*Define a few values of type card: the Ace of Clubs, the Queen of Hearts, the Two of Diamonds, the Seven of Spades.*)
let ace_clubs = { suit = Clubs; rank = Ace};;
let queen_hearts = { suit = Hearts; rank = Queen};;
let two_diamonds = { suit = Diamonds; rank = Two};;
let seven_spades = { suit = Spades; rank = Seven};;


(*// A value of type 'int option list' is a list where each element can either be an integer or absent. //*)

(*The quadrant function below, returns the quadrant of the given x, y point of a diagram.*)
type quad = I | II | III | IV;;
type sign = Neg | Zero | Pos;;
(*The sign function determines the sign of an integer ('Neg' for negative, 'Zero' for zero, and 'Pos' for positive.*)
let sign (x: int) : sign = 
  if x < 0 then Neg
  else if x > 0 then Pos
  else Zero ;;
(*val sign : int -> sign = <fun>*)

let quadrant : int * int -> quad option = fun (x,y) ->
  let x_sign = sign x in
  let y_sign = sign y in
  match (x_sign, y_sign) with
  | (Pos, Pos) -> Some I
  | (Neg, Pos) -> Some II
  | (Neg, Neg) -> Some III
  | (Pos, Neg) -> Some IV
  | (_, Zero) | (Zero,_) -> None;;
(*val quadrant : int * int -> quad option = <fun>*)

(*Rewriting the quadrant function to use the 'when' syntax. We won’t need the helper function from before.*)
let quadrant_when : int * int -> quad option = function
| (x, y) when x > 0 && y > 0 -> Some I
| (x, y) when x < 0 && y > 0 -> Some II
| (x, y) when x < 0 && y < 0 -> Some III
| (x, y) when x > 0 && y < 0 -> Some IV
| _ -> None;;
(*val quadrant_when : int * int -> quad option = <fun>*)


(*A function that returns the number of nodes in any longest path from the root to a leaf.*)
type 'a tree = 
| Leaf 
| Node of 'a * 'a tree * 'a tree;;
(*type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree*)

let rec depth tree = match tree with
| Leaf -> 0
| Node (_, left, right) -> 1 + max (depth left) (depth right);;
(*val depth : 'a tree -> int = <fun>*)
(*let example_tree = Node(10, Node (5,Leaf, Leaf), Node (15, Node (12, Leaf, Leaf), Leaf));; *)

let rec same_shape tree1 tree2 = 
  match (tree1, tree2) with
  | (Leaf, Leaf) -> true
  | (Node (_, left1, right1), Node (_, left2, right2)) ->
    same_shape left1 left2 && same_shape right1 right2
  | _ -> false;;
(*val same_shape : 'a tree -> 'b tree -> bool = <fun>*)
(*Example - *)
let tree1 = Node (1, Leaf, Leaf);;
let tree2 = Node (2, Node (3, Leaf, Leaf), Leaf);;
let tree3 = Node ("A", Node ("B", Leaf, Leaf), Node ("C", Leaf, Leaf));;


(*A function that returns the maximum integer in a list.*)
let rec list_max = function 
| [] -> raise (Failure "")
| hd :: tl -> List.fold_left max hd tl;;
(*val list_max : 'a list -> 'a = <fun>*)

(*A function hat returns a string containing the maximum integer in a list.*)
let rec list_max_string = function
| [] -> "empty"
| hd :: tl -> string_of_int(List.fold_left max hd tl);;
(*val list_max_string : int list -> string = <fun>*)


(*A function that returns true if and only if the given tree satisfies the binary search tree invariant.*)
type ('a, 'b) tree = 
| Leaf 
| Node of 'a * 'b * ('a, 'b) tree * ('a, 'b) tree;;
(*type ('a, 'b) tree = Leaf | Node of 'a * 'b * ('a, 'b) tree * ('a, 'b) tree*)

type ('a,'b) bst_info = 
| Empty
| Value of 'a * 'b
| Invalid;;
(*type ('a, 'b) bst_info = Empty | Value of 'a * 'b | Invalid*)

let rec bst_info tree = match tree with
| Leaf -> Empty
| Node (key, _, left, right) -> 
  let min_info = bst_info left in
  let max_info = bst_info right in
  match (min_info, max_info) with 
  | (Value (min_key, _), Value (_, max_key)) when min_key <= key && key <= max_key -> Value (min_key, max_key)
  | _ -> Invalid;;
(*val bst_info : ('a, 'b) tree -> ('a, 'a) bst_info = <fun>*)

let is_bst tree = match bst_info tree with
| Value _ -> true
| _ -> false;;
(*val is_bst : ('a, 'b) tree -> bool = <fun>*)

(*Example - *)
let tree4 = Node (2, "Two", Node (1, "One", Leaf, Leaf), Leaf);;
let tree5 = Node (2, "Two", Node (3, "Three", Leaf, Leaf), Node (1, "One", Leaf, Leaf));;


(*Modify the definition of quadrant to use polymorphic variants. The types of your functions should become these:
val sign : int -> [> `Neg | `Pos | `Zero ]
val quadrant : int * int -> [> `I | `II | `III | `IV ] option *)

type sign1 = [ `Neg | `Pos | `Zero ];;
type quad1 = [ `I | `II | `III | `IV ];;

let sign1 (x: int) : [> `Neg | `Pos | `Zero ] = 
  if x < 0 then `Neg
  else if x > 0 then `Pos
  else `Zero ;;
(*val sign1 : int -> [> `Neg | `Pos | `Zero ] = <fun> *)

let quadrant1 : int * int -> [> `I | `II | `III | `IV ] option = fun (x, y) ->
  let x_sign = sign1 x in
  let y_sign = sign1 y in
  match (x_sign, y_sign) with
  | (`Pos, `Pos) -> Some `I
  | (`Neg, `Pos) -> Some `II
  | (`Neg, `Neg) -> Some `III
  | (`Pos, `Neg) -> Some `IV
  | (`Zero, `Zero) | _ -> None;;
(*val quadrant1 : int * int -> [> `I | `II | `III | `IV ] option = <fun> *)
(*Example - *)
quadrant1 (-1, 5);;

(*Rewriting the quadrant function to use the 'when' syntax. We won’t need the helper function from before.*)
let quadrant_when1 : int * int -> [> `I | `II | `III | `IV ] option = function
| (x, y) when x > 0 && y > 0 -> Some `I
| (x, y) when x < 0 && y > 0 -> Some `II
| (x, y) when x < 0 && y < 0 -> Some `III
| (x, y) when x > 0 && y < 0 -> Some `IV
| _ -> None;;
(*val quadrant_when1 : int * int -> [> `I | `II | `III | `IV ] option = <fun>*)
(*Exmaple - *)
quadrant_when1 (6, -9);;

(* ======================================================================================================================================= *)
