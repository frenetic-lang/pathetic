open OpenFlow0x01Types
open NetCoreFT

(* type graph = (switchId * switchId * int) list *)


type regex =
  | Hop of switchId
  | Host of int
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
  | Hop sw -> Printf.sprintf "(Hop %Ld)" sw
  | Host h -> Printf.sprintf "(Host %d)" h
  | Star -> "*"
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
let reduce_re re = match re with
  | Empty -> Empty
  | Intersection(Empty, Empty) -> Empty
  | Union(Empty, _) -> Empty
  | Union(_, Empty) -> Empty
  | _ -> EmptySet

let rec nu re = match re with
  | Empty -> Empty
  | Hop _ -> EmptySet
  | Host _ -> EmptySet
  | EmptySet -> EmptySet
  | Sequence(a,b) -> reduce_re (nu a && nu b)
  | Union(a,b) -> reduce_re (nu a || nu b)
  | Star -> Empty
  | Intersection(a,b) -> reduce_re (nu a && nu b)
  | Not a -> match nu a with
      | Empty -> EmptySet
      | EmptySet -> Empty

let rec deriv sym re = match re with
  | Empty -> EmptySet
  | EmptySet -> EmptySet
  | Host a -> if sym = Host a then Empty else EmptySet
  | Hop a -> if sym = Hop a then Empty else EmptySet
  | Sequence(a,b) -> ((deriv sym a) <.> b) || ((nu a) <.> deriv sym b)
  | Star -> Star
  | Union(a,b) -> (deriv sym a) || (deriv sym b)
  | Intersection(a,b) -> (deriv sym a) && (deriv sym b)
  | Not a -> Not (deriv sym a)

let rec deriv_path path re = List.fold_left (fun x y -> deriv y x) re (List.rev path)

let rec match_path re path = match path with
  | [] -> nu re = Empty
  | a :: w -> match_path (deriv a re) w
