(* type graph = (switchId * switchId * int) list *)

type regex =
    Hop of OpenFlow0x01Types.switchId
  | Host of int
  | Star
  | Sequence of regex * regex
  | Union of regex * regex
  | Intersection of regex * regex
  | Not of regex
  | Empty
  | EmptySet

type regex_policy =
    RegPol of NetCoreFT.predicate * regex * int
  | RegUnion of regex_policy * regex_policy
  | RegInter of regex_policy * regex_policy

val ( <+> ) : regex_policy -> regex_policy -> regex_policy
val ( <*> ) : regex_policy -> regex_policy -> regex_policy
val ( && ) : regex -> regex -> regex
val ( || ) : regex -> regex -> regex
val ( <.> ) : regex -> regex -> regex

val regex_to_string : regex -> string
val nu : regex -> regex
val match_path : regex -> regex list -> bool
val deriv : regex -> regex -> regex
