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

let rec height pol = match pol with
  | RegPol _ -> 1
  | RegUnion(pol1, pol2) -> 1 + max (height pol1) (height pol2)
  | RegInter(pol1, pol2) -> 1 + max (height pol1) (height pol2)

(* inter over union *)

let rec cnf_form pol = match pol with
  | RegPol _ -> true
  | RegUnion(RegPol _, RegPol _) -> true
  | RegUnion(RegPol _, RegUnion _) -> false
  | RegUnion(RegUnion _, RegPol _) -> false
  | RegUnion(RegInter _, _) -> false
  | RegUnion(_, RegInter _) -> false
  | RegUnion(a,b) -> cnf_form a & cnf_form b
  | RegInter(RegPol _, RegPol _) -> true
  | RegInter(RegPol _, RegUnion _) -> false
  | RegInter(RegUnion _, RegPol _) -> false
  | RegInter(RegInter _, RegPol _) -> false
  | RegInter(RegPol _, RegInter _) -> false
  | RegInter(a,b) -> cnf_form a & cnf_form b

let rec dnf_form pol = match pol with
  | RegPol _ -> true
  | RegInter(RegPol _, RegPol _) -> true
  | RegInter(RegPol _, RegInter _) -> false
  | RegInter(RegInter _, RegPol _) -> false
  | RegInter(RegUnion _, _) -> false
  | RegInter(_, RegUnion _) -> false
  | RegInter(a,b) -> dnf_form a & dnf_form b
  | RegUnion(RegPol _, RegPol _) -> true
  | RegUnion(RegPol _, RegInter (a,b)) -> dnf_form (RegInter (a,b))
  | RegUnion(RegInter (a,b), RegPol _) -> dnf_form (RegInter (a,b))
  | RegUnion(RegUnion _, RegPol _) -> false
  | RegUnion(RegPol _, RegUnion _) -> false
  | RegUnion(a,b) -> dnf_form a & dnf_form b

let rec to_cnf' pol = match pol with
  | RegUnion (RegPol _, RegPol _) -> pol
  | RegUnion (RegInter(a,b), c) -> RegInter(to_cnf' (RegUnion(a,c)), to_cnf' (RegUnion(b,c)))
  | RegUnion (c,RegInter(a,b)) -> RegInter(to_cnf' (RegUnion(a,c)), to_cnf' (RegUnion(b,c)))
  | RegUnion (RegUnion (pol1,pol2), RegPol(pr,re,k) ) -> 
    let pol3 = RegPol(pr,re,k) in
    RegUnion (to_cnf' (RegUnion(pol1, pol3)), 
	      to_cnf' (RegUnion(pol2, pol3)))
  | RegUnion (RegPol(pr,re,k), RegUnion (pol1,pol2)) -> 
    let pol3 = RegPol(pr,re,k) in
    RegUnion (to_cnf' (RegUnion(pol1, pol3)), 
	      to_cnf' (RegUnion(pol2, pol3)))
  | RegUnion (a,b) -> RegUnion(to_cnf' a, to_cnf' b)

let rec to_dnf' pol = match pol with
  | RegInter (RegPol _, RegPol _) -> pol
  | RegInter (RegUnion(a,b), c) -> RegUnion(to_dnf' (RegInter(a,c)), to_dnf' (RegInter(b,c)))
  | RegInter (c,RegUnion(a,b)) -> RegUnion(to_dnf' (RegInter(a,c)), to_dnf' (RegInter(b,c)))
  | RegInter (RegInter (pol1,pol2), RegPol(pr,re,k) ) -> 
    let pol3 = RegPol(pr,re,k) in
    RegInter (to_dnf' (RegInter(pol1, pol3)), 
	      to_dnf' (RegInter(pol2, pol3)))
  | RegInter (RegPol(pr,re,k), RegInter (pol1,pol2)) -> 
    let pol3 = RegPol(pr,re,k) in
    RegInter (to_dnf' (RegInter(pol1, pol3)), 
	      to_dnf' (RegInter(pol2, pol3)))
  | RegInter (a,b) -> RegInter(to_dnf' a, to_dnf' b)
  | RegUnion (RegPol _, RegPol _) -> pol
  | RegUnion (RegUnion(a,b), RegPol (pr,re,k)) -> RegUnion( to_dnf' (RegUnion (a, (RegPol (pr, re, k)))),
							    to_dnf' (RegUnion (b, (RegPol (pr, re, k)))))
  | RegUnion (RegPol (pr,re,k), RegUnion(a,b)) -> RegUnion( to_dnf' (RegUnion (a, (RegPol (pr, re, k)))),
							    to_dnf' (RegUnion (b, (RegPol (pr, re, k)))))
  | RegUnion (a, b) -> RegUnion( to_dnf' a, to_dnf' b)
  | RegPol _ -> pol

let rec to_cnf pol = 
  let pol' = to_cnf' pol in
  if cnf_form pol' then pol'
  else to_cnf pol'

let rec to_dnf pol = 
  let pol' = to_dnf' pol in
  if dnf_form pol' then pol'
  else to_dnf pol'

let rec blast_inter pol = match pol with
  | RegInter(RegPol(pr1,re1,k1), RegPol(pr2,re2,k2)) ->
    RegPol(And(pr1,pr2), re1 && re2, max k1 k2)
  | RegInter(a, b) -> let RegPol(pr1, re1, k1) = blast_inter a in
		      let RegPol(pr2, re2, k2) = blast_inter b in
		      RegPol(And(pr1,pr2), re1 && re2, max k1 k2)
  | RegUnion(a, b) -> RegUnion(blast_inter a, blast_inter b)
  | RegPol _ -> pol

let rec product lst1 lst2 = match lst1 with
  | [] -> []
  | a :: lst1 -> (List.map (fun x -> (a,x)) lst2) @ product lst1 lst2

(* pr2 is an atomic pred *)
let rec pred_matches pr1 pr2 = 
  match pr1 with
    | All -> true
    | NoPackets -> false
    | And(a,b) -> pred_matches a pr2 & pred_matches b pr2
    | Or(a,b) -> pred_matches a pr2 or pred_matches b pr2
    | Not a -> not (pred_matches a pr2)
    | _ -> pr1 = pr2

let rec preds_overlap pr1 pr2 = match pr1 with
  | All -> not (is_empty_pred pr2)
  | NoPackets -> false
  | DlType a -> pred_matches pr2 (DlType a)
  | Or(a,b) -> preds_overlap a pr2 or preds_overlap b pr2
  | Not a -> not (preds_overlap a (Not pr2))
  | And(a,b) -> preds_overlap a (And(b,pr2)) & preds_overlap b (And(a,pr2))
and
    is_empty_pred pr = match pr with
      | NoPackets -> true
      | All -> false
      | DlType _ -> false
      | Or(a,b) -> is_empty_pred a & is_empty_pred b
      | Not a -> not (is_empty_pred a)
      | And(a,b) -> not (preds_overlap a b)


let rec blast_union1 (pol1, pol2) = match pol1,pol2 with
  | RegPol(pr1,re1,k1), RegPol(pr2,re2,k2) ->
    [RegPol(And (pr1, Not pr2), re1, k1);
     RegPol(And (pr2, Not pr1), re2, k2);
     RegPol(And (pr1, pr2), re1 && re2, max k1 k2)]

let rec blast_union pol = match pol with
  | RegUnion(a,b) -> let prod = product (blast_union a) (blast_union b) in
		       List.concat (List.map blast_union1 prod)
  | RegPol _ -> [pol]

let rec normalize pol = 
  blast_union (blast_inter (to_dnf pol))

let rec compile_regex pol topo = match pol with
  | RegPol (pred, reg, _) -> compile_path pred (expand_re reg topo) topo (Gensym.next ())
  | RegUnion (pol1, pol2) -> Par (compile_regex pol1 topo, compile_regex pol2 topo)
