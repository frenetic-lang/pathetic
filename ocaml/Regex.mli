(* type graph = (switchId * switchId * int) list *)

type regex =
    Const of Graph.Graph.node
  | Star
  | Sequence of regex * regex
  | Union of regex * regex
  | Intersection of regex * regex
  | Comp of regex
  | Empty
  | EmptySet

type regex_policy =
    RegPol of NetCore_Types.pred * regex
  | RegUnion of regex_policy * regex_policy
  | RegInter of regex_policy * regex_policy

val ( <+> ) : regex_policy -> regex_policy -> regex_policy
val ( <*> ) : regex_policy -> regex_policy -> regex_policy
val ( && ) : regex -> regex -> regex
val ( || ) : regex -> regex -> regex
val ( <.> ) : regex -> regex -> regex

val regex_to_string : regex -> string
val regexPol_to_string : regex_policy -> string
val is_empty : regex -> bool
val match_path : regex -> regex list -> bool
val deriv : regex -> regex -> regex
