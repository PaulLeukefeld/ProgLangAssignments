(* This is an OCAML comment *)
(*
   You should write your functions in this file.
   All your functions should have their parameter types specified.
   Write your code right below the corresponding comment describing the
   function you are asked to write.
*)


(*
   Write a function named "fixLastTwo" that takes a triple of integers
   (x, y, z) and possibly rearranges the last two so that they are in increasing
   order (and just keeps the first in place).
   It should have type: int * int * int -> int * int * int
*)

let fixLastTwo ((x, y, z) : int * int * int) : int * int * int =
   let result = if y > z then (x, z, y) else (x, y, z) in
   result;;

fixLastTwo(1, 2, 3);;

(*
   Write a function named "order" that takes a triple of integers and
   returns a triple of the same integers but in increasing order.
   You may want to use the function from the previous part.
   It should have type: int * int * int -> int * int * int
*)
let order ((x, y, z) : int * int * int) : int * int * int =
   let result = if y > z && z > x then (x, z, y) else if x > y && y > z then (z, y, x) else if z > y && y > x then (x, y, z) else (z, x, y) in
   result;;

order(1, 2, 3);;


(*
   Write a function "distance" that given a pair of integers returns the
   distance between them. For instance the distance between 10 and 4 is 6,
   as is the distance between 4 and 10.
   It should have type: int * int -> int
*)
let distance ((x, y) : int * int) : int =
   let calculation_result = x - y in 
   let result = if calculation_result < 0 then calculation_result * -1 else calculation_result in
   result;;

distance(4, 10);;


(*
   Write a function "greeting" that given a pair of an integer (age) and
   a string (name) creates the string: "Greetings <name>, you are <age> years old!".
   You will need to look in the Pervasives module for "string concatenation"
   and for how to convert an int to a string (NOT to a char).
   It should have type: int * string -> string
   You may see "bytes" instead of "string" as a type.
*)

let greeting ((age, name) : int * string) : string = 
   let result = "Greetings " ^ name ^ ", you are " ^ string_of_int(age) ^ " years old!" in
   result;;   

greeting(21, "Paul");;


(*
   Write a function "greeting2" that is similarly given a pair of an integer (age)
   and a string (name) and creates the string: "Greetings <name>, you are ..." where
   the dots depend on the age:
   - If the age is 0 or less, it should say "not born yet!".
   - If the age is 1 to 20, it should say "a youngster!".
   - If the age is more than 20, it should say "young at heart!".
   It should have type: int * string -> string
   You may see "bytes" instead of "string" as a type.
*)

let greeting2 ((age, name) : int * string) : string =
   let age_value = if age <= 0 then "not born yet" else if age <= 20 then "a youngster" else "young at heart!" in
   let result = "Greetings " ^ name ^ ", you are " ^ age_value in  
   result;;

greeting2(22, "Paul");;


(*
   Write a function "tooShort" that is given a pair of an integer and a string
   and returns a boolean indicating whether that integer is strictly larger than
   the length of the string. The function "String.length" returns the length of
   a string.
   It should have type: int * string -> bool
*)

let tooShort ((number, word) : int * string) : bool =
   let result = if number > String.length(word) then true else false in
   result;;
   
tooShort(2, "two");;


(*
   Write a function "totalLength" that is given a pair of strings and returns
   their total length.
   It should have type string * string -> int
*)

let totalLength ((first_word, second_word) : string * string) : int =
   let combined_string = first_word ^ second_word in
   let result = String.length(combined_string) in
   result;;

totalLength("Hello", "World");;


(*
   Write a function "orderedByLength" that is given a triple of strings and returns
   a boolean indicating whether the strings are ordered in increasing length. For a
   fully correct solution, your code should not compute the length of a specific
   string more than once.
   It should have type: string * string * string -> bool
*)

let orderedByLength ((first_word, second_word, third_word) : string * string * string) : bool =
   let length_first_word = String.length(first_word) in
   let length_second_word = String.length(second_word) in
   let length_third_word = String.length(third_word) in
   let result = if length_second_word < length_third_word && length_first_word < length_second_word then true else false in
   result;;

orderedByLength("one", "three", "eleven");;


(*
   Write a function "prodInRange" that is given a pair of integers, and it returns
   a boolean indicating whether their product is strictly between 10 and 20. For a
   fully correct solution, your code should not compute the product of the two
   integers more than once.
   It should have type: int * int -> bool
*)

let prodInRange ((first_number, second_number) : int * int) : bool =
   let calculation_result = first_number * second_number in
   let smaller_check = if calculation_result < 10 then false else true in
   let larger_check = if calculation_result > 20 then false else true in
   let result = if smaller_check && larger_check then true else false in
   result;;

prodInRange(2, 7);;