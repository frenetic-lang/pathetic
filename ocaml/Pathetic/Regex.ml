open OpenFlow0x01Types
open NetCoreFT
module G = Graph.Graph

(* type graph = (switchId * switchId * int) list *)


type regex =
  | Const of G.node
  | Star
  | Sequence of regex * regex
  | Union of regex * regex
  | Intersection of regex * regex
  | Not of regex
  | Empty
  | EmptySet

type regex_policy = 
  | RegPol of predicate * regex * int
  | RegUnion of regex_policy * regex_policy
  | RegInter of regex_policy * regex_policy

let (<+>) a b = RegUnion(a,b)
let (<*>) a b = RegInter(a,b)

let (&&) a b = Intersection(a,b)
let (||) a b = Union(a,b)
let (<.>) a b = Sequence(a,b)

let rec regex_to_string reg = match reg with
  | Const(h) -> Printf.sprintf "%s" (G.node_to_string h)
  | Star -> "*"
  | Empty -> "Empty"
  | EmptySet -> "{}"
  | Not r -> Printf.sprintf "not (%s)" (regex_to_string r)
  | Sequence(reg1, reg2) -> Printf.sprintf "( %s <.> %s )" (regex_to_string reg1) (regex_to_string reg2)
  | Union(reg1, reg2) -> Printf.sprintf "( %s <||> %s )" (regex_to_string reg1) (regex_to_string reg2)
  | Intersection(reg1, reg2) -> Printf.sprintf "( %s <&&> %s )" (regex_to_string reg1) (regex_to_string reg2)

let rec regexPol_to_string regPol = match regPol with
  | RegPol(pr, reg, k) -> Printf.sprintf "RegPol(%s, %s, %d)" (predicate_to_string pr) (regex_to_string reg) k
  | RegUnion(reg_pol1, reg_pol2) -> Printf.sprintf "(%s <+> %s)" (regexPol_to_string reg_pol1) (regexPol_to_string reg_pol2)
  | RegInter(reg_pol1, reg_pol2) -> Printf.sprintf "(%s <*> %s)" (regexPol_to_string reg_pol1) (regexPol_to_string reg_pol2)

(* Normalization algorithm:
   1) Push intersections till they're over atomic policies or other intersection (DNF)
   2) Eliminate intersections
   3) Find a minimal independent covering set
*)

(* Need to simplify by pushing empty/emptyset up *)
let reduce_nu re = match re with
  | Empty -> Empty
  | Intersection(Empty, Empty) -> Empty
  | Union(Empty, _) -> Empty
  | Union(_, Empty) -> Empty
  | _ -> EmptySet

let rec reduce_re re = match re with
      | Empty -> Empty
      | Const c -> Const c
      | EmptySet -> EmptySet
      | Sequence(Empty,b) -> reduce_re b
      | Sequence(a,Empty) -> reduce_re a
      | Sequence(EmptySet,_) -> EmptySet
      | Sequence(_,EmptySet) -> EmptySet
      | Sequence(a,b) -> Sequence(reduce_re a, reduce_re b)
      | Union(EmptySet,b) -> reduce_re b
      | Union(a,EmptySet) -> reduce_re a
      | Union(Star,b) -> Star
      | Union(a,Star) -> Star
      | Union(a,b) -> if a = b then reduce_re a else Union(reduce_re a, reduce_re b)
      | Star -> Star
      | Intersection(Star,b) -> reduce_re b
      | Intersection(a,Star) -> reduce_re a
      | Intersection(EmptySet,_) -> EmptySet
      | Intersection(_,EmptySet) -> EmptySet
      | Intersection(a,b) -> if a = b then reduce_re a else Intersection(reduce_re a, reduce_re b)
      | Not (Not a) -> reduce_re a
      | Not a -> Not (reduce_re a)

let rec nu re = 
  let foo =
    match re with
      | Empty -> Empty
      | Const _ -> EmptySet
      | EmptySet -> EmptySet
      | Sequence(a,b) -> reduce_nu (nu a && nu b)
      | Union(a,b) -> reduce_nu (nu a || nu b)
      | Star -> Empty
      | Intersection(a,b) -> reduce_nu (nu a && nu b)
      | Not a -> match nu a with
	  | Empty -> EmptySet
	  | EmptySet -> Empty
  in
  (* Printf.printf "nu %s\n" (regex_to_string re); *)
  (* Printf.printf "returned %s\n" (regex_to_string foo); *)
  foo

let rec is_empty re = match re with
  | Empty -> false
  | EmptySet -> true
  | Const _ -> false
  | Sequence(a,b) -> is_empty(a) or is_empty(b)
  | Union(a,b) -> is_empty(a) & is_empty(b)
  | Star -> false
  | Not a -> not (is_empty a)

let rec deriv sym re = 
  let return =  match re with
    | Empty -> EmptySet
    | EmptySet -> EmptySet
    | Const c -> if sym = Const c then Empty else EmptySet
    | Sequence(a,b) -> reduce_re (((deriv sym a) <.> b) || ((nu a) <.> deriv sym b))
    | Star -> Star
    | Union(a,b) -> reduce_re ((deriv sym a) || (deriv sym b))
    | Intersection(a,b) -> reduce_re ((deriv sym a) && (deriv sym b))
    | Not a -> reduce_re (Not (deriv sym a))
  in
  (* Printf.printf "deriv %s %s\n" (regex_to_string sym) (regex_to_string re); *)
  (* Printf.printf "returned %s\n" (regex_to_string return); *)
  return

let rec deriv_path path re = List.fold_left (fun x y -> deriv y x) re (List.rev path)

let rec match_path re path = 
  let return = match path with
    | [] -> nu re = Empty
    | a :: w -> match_path (deriv a re) w
  in
  (* Printf.printf "match_path %s %s\n" (regex_to_string re)  *)
  (*   (String.concat ";" (List.map regex_to_string path)); *)
  (* Printf.printf "returned %B\n" return; *)
  return 



