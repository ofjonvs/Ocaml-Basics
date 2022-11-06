(***********************************)
(* Part 1: Non-Recursive Functions *)
(***********************************)

let rev_tup tup = let (a, b, c) = tup in (c, b, a)

let is_odd x = if x mod 2 = 0 then false else true

let area x y = match x, y with (a, b), (c, d) ->
  if (c - a) * (d - b) > 0 then (c - a) * (d - b)
  else -((c - a) * (d - b))

let volume x y = match x, y with (a, b, c), (d, e, f) ->
  if ((d-a) * (e-b) * (f-c)) > 0 then (d-a) * (e-b) * (f-c)
  else -((d-a) * (e-b) * (f-c))

(*******************************)
(* Part 2: Recursive Functions *)
(*******************************)

let rec fibonacci n = match n with
0 -> 0
|1 -> 1
|2 -> 1
|x -> (fibonacci (x - 1)  +  fibonacci(x - 2));;

let rec pow x y = match y with
0 -> 1
|1 -> x
|_ -> x * pow x (y-1)

let rec log x y = if x<= y then 1 + (log x (y / x)) else 0

let rec gcf x y = if y = 0 then x else gcf y (x mod y)

let rec fold f a l = match l with | []-> a | h::t-> fold f (f a h) t
let rec is_prime x = if x < 2 then false else
  let rec temp div = if div < x then div :: temp (div+1)
  else [] in fold (fun a h -> a && (x mod h) !=0) true (temp 2)


(*****************)
(* Part 3: Lists *)
(*****************)

let rec get idx lst = match idx, lst with
0, h::t -> h
|_, h::t -> get (idx - 1) t 
|0, _ -> failwith "Out of bounds"
|_, [] -> failwith "Out of bounds"

let larger lst1 lst2 = let rec length lst = match lst with 
[]-> 0 
|_::t-> 1+(length t)
in if length lst1 = length lst2 then [] else if length lst1 > length lst2 then lst1 else lst2

let reverse lst = let rec aux accumulator = function
h::t -> aux (h::accumulator) t
|[] -> accumulator in aux [] lst


let rec combine lst1 lst2 = match lst1, lst2 with
h1 :: t1, h2 :: t2 -> h1::combine t1 lst2
| [], _ -> lst2
| _, [] -> lst1

let rec merge lst1 lst2 = match lst1, lst2 with
h1 :: tlst1, h2 :: tlst2 -> if h1 < h2 then h1 :: merge tlst1 lst2 else h2 :: merge lst1 tlst2
| [], _ -> lst2
| _, [] -> lst1


let rec rotate shift lst = match shift, lst with
0, _ -> lst
|_, [] -> []
|_, h::t -> rotate (shift-1) (t@(h::[]))

let rec is_palindrome lst = if reverse lst = lst then true else false