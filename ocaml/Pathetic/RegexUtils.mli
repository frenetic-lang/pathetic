open Regex

val compile_regex : regex_policy -> Graph.Graph.graph -> NetCoreFT.policy
val shortest_path_re : regex -> Graph.node -> Graph.Graph.graph -> Graph.node list
val expand_re : regex -> Graph.Graph.graph -> Graph.node list
val to_cnf : regex_policy -> regex_policy
val to_dnf : regex_policy -> regex_policy
val blast_inter : regex_policy -> regex_policy
val normalize : regex_policy -> regex_policy list
