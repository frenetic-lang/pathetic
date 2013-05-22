open Regex
module G = Graph
open NetCoreFT

  (* Naive compilation: does not guarantee loop-free semantics
     Possible issues:
     1) reg contains an explicit loop
     2) We compile star paths to contain the same node

     Possible solutions:
     1) Second compilation phase that detects repeated nodes and tags packets inbetween such repeats
  *)

module Q = Queue

(* Need to add constraints to avoid routing through hosts *)
let rec bfs' graph queue =
  let (sw, re, path) = (Q.take queue) in
  (* Printf.printf "bfs' %s %s %s\n" (G.Graph.node_to_string sw) (regex_to_string re)  *)
  (*   (String.concat ";" (List.map G.Graph.node_to_string path)); *)
  match (match_path re [Const sw]) with
    | true -> path
    | false -> 
      List.iter (fun x -> 
	let re' = deriv (Const x) re in
	match is_empty re' with
	  | false ->
	    Q.add (x, re', x :: path) queue
	  | true -> ()) (G.Graph.get_nbrs graph sw); 
      G.Graph.del_node graph sw;
      bfs' graph queue


let bfs graph re = 
  let q = Queue.create () in
  List.iter (fun src ->
    let re' = deriv (Const src) re in
    match is_empty re' with
      | false -> Q.add (src, re', [src]) q
      | true -> ())
    (G.Graph.get_nodes graph);
  (try (bfs' (G.Graph.copy graph) q) 
   with Queue.Empty -> raise G.Graph.(NoPath("unknown", "unknown")))


let expand_re re topo = 
  Printf.printf "expand_re %s\n" (regex_to_string re);
  try let return = List.rev (bfs topo re) in
      Printf.printf "expand_re returned %s\n" (String.concat ";" (List.map G.Graph.node_to_string return));
      return
  with 
    | G.Graph.NoPath(s1,s2) -> Printf.printf "Couldn't find path for %s in graph\n\t%s\n" (regex_to_string re) (G.Graph.to_string topo);
      raise (G.Graph.(NoPath("unknown","unknown")))

let shortest_path_re re src topo = 
  Printf.printf "shortest_path_re %s %s\n" (regex_to_string re) (G.Graph.node_to_string src);
  let q = Queue.create () in
  let re' = deriv (Const src) re in
  (match is_empty re' with
    | false -> Q.add (src, re', [src]) q
    | true -> ());
  (try List.rev (bfs' (G.Graph.copy topo) q) 
   with Queue.Empty -> raise G.Graph.(NoPath("unknown", "unknown")))
 
let rec compile_path1 pred path topo port = match path with
  | G.Switch s1 :: G.Switch s2 :: path -> 
    let p1,p2 = G.Graph.get_ports topo (G.Switch s1) (G.Switch s2) in
    Par ((Pol ((And (pred, (And (InPort port,Switch s1)))), [To (unmodified, p1)])), ((compile_path1 pred ((G.Switch s2) :: path) topo p2)))
  | G.Switch s1 :: [G.Host h] -> 
    let p1,_ = G.Graph.get_ports topo (G.Switch s1) (G.Host h) in
    Pol ((And (pred, (And (InPort port,Switch s1)))), 
	 [To ({unmodified with NetCoreEval0x04.modifyDlVlan=(Some None)}, p1)])
  | _ -> Pol (pred, [])

let compile_path pred path topo (vid : WordInterface.Word16.t)  = match path with
  | G.Host h1 :: G.Switch s :: [G.Host h2] -> 
    let (_,p1) = G.Graph.get_ports topo (G.Host h1) (G.Switch s) in 
    let (p2,_) = G.Graph.get_ports topo (G.Switch s) (G.Host h2) in
    Pol ((And (pred, (And (InPort p1,Switch s)))), [To (unmodified, p2)])
  | G.Host h :: G.Switch s1 :: G.Switch s2 :: path -> 
    let _,inport = G.Graph.get_ports topo (G.Host h) (G.Switch s1) in
    let p1,p2 = G.Graph.get_ports topo (G.Switch s1) (G.Switch s2) in
    let pol = Pol (And (pred, (And (InPort inport,Switch s1))), 
		   [To ({unmodified with NetCoreEval0x04.modifyDlVlan=(Some (Some vid))}, p1)]) in
    Par (pol, compile_path1 (And (DlVlan (Some vid), And (pred, DlVlanPcp 0))) (G.Switch s2 :: path) topo p2)



module Gensym =
struct
  let count = ref 0
  let next () = incr count; !count
end

let rec compile_regex pol topo = match pol with
  | RegPol (pred, reg, _) -> compile_path pred (expand_re reg topo) topo (Gensym.next ())
  | RegUnion (pol1, pol2) -> Par (compile_regex pol1 topo, compile_regex pol2 topo)
