open Regex

val compile_regex : regex_policy -> Graph.Graph.graph -> NetCoreFT.policy
val shortest_path_re : regex -> Graph.node -> Graph.Graph.graph -> Graph.node list
val expand_re : regex -> Graph.Graph.graph -> Graph.node list
