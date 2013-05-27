open Regex
module G = Graph.Graph
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
  (* Printf.printf "bfs' %s %s %s\n" (G.node_to_string sw) (regex_to_string re)  *)
  (*   (String.concat ";" (List.map G.node_to_string path)); *)
  match (match_path re [Const sw]) with
    | true -> path
    | false -> 
      List.iter (fun x -> 
	let re' = deriv (Const x) re in
	match is_empty re' with
	  | false ->
	    Q.add (x, re', x :: path) queue
	  | true -> ()) (G.get_nbrs graph sw); 
      G.del_node graph sw;
      bfs' graph queue


let bfs graph re = 
  let q = Queue.create () in
  List.iter (fun src ->
    let re' = deriv (Const src) re in
    match is_empty re' with
      | false -> Q.add (src, re', [src]) q
      | true -> ())
    (G.get_nodes graph);
  (try (bfs' (G.copy graph) q) 
   with Queue.Empty -> raise G.(NoPath("unknown", "unknown")))


let expand_re re topo = 
  Printf.printf "expand_re %s\n" (regex_to_string re);
  try let return = List.rev (bfs topo re) in
      Printf.printf "expand_re returned %s\n" (String.concat ";" (List.map G.node_to_string return));
      return
  with 
    | G.NoPath(s1,s2) -> Printf.printf "Couldn't find path for %s in graph\n\t%s\n" (regex_to_string re) (G.to_string topo);
      raise (G.(NoPath("unknown","unknown")))

let shortest_path_re re src topo = 
  Printf.printf "shortest_path_re %s %s\n" (regex_to_string re) (G.node_to_string src);
  let q = Queue.create () in
  let re' = deriv (Const src) re in
  (match is_empty re' with
    | false -> Q.add (src, re', [src]) q
    | true -> ());
  (try List.rev (bfs' (G.copy topo) q) 
   with Queue.Empty -> raise G.(NoPath("unknown", "unknown")))
 
let rec compile_path1 pred path topo port = match path with
  | G.Switch s1 :: G.Switch s2 :: path -> 
    let p1,p2 = G.get_ports topo (G.Switch s1) (G.Switch s2) in
    Par ((Pol ((And (pred, (And (InPort port,Switch s1)))), [To (unmodified, p1)])), ((compile_path1 pred ((G.Switch s2) :: path) topo p2)))
  | G.Switch s1 :: [G.Host h] -> 
    let p1,_ = G.get_ports topo (G.Switch s1) (G.Host h) in
    Pol ((And (pred, (And (InPort port,Switch s1)))), 
	 [To ({unmodified with NetCoreEval0x04.modifyDlVlan=(Some None)}, p1)])
  | _ -> Pol (pred, [])

let compile_path pred path topo (vid : WordInterface.Word16.t)  = match path with
  | G.Host h1 :: G.Switch s :: [G.Host h2] -> 
    let (_,p1) = G.get_ports topo (G.Host h1) (G.Switch s) in 
    let (p2,_) = G.get_ports topo (G.Switch s) (G.Host h2) in
    Pol ((And (pred, (And (InPort p1,Switch s)))), [To (unmodified, p2)])
  | G.Host h :: G.Switch s1 :: G.Switch s2 :: path -> 
    let _,inport = G.get_ports topo (G.Host h) (G.Switch s1) in
    let p1,p2 = G.get_ports topo (G.Switch s1) (G.Switch s2) in
    let pol = Pol (And (pred, (And (InPort inport,Switch s1))), 
		   [To ({unmodified with NetCoreEval0x04.modifyDlVlan=(Some (Some vid))}, p1)]) in
    Par (pol, compile_path1 (And (DlVlan (Some vid), And (pred, DlVlanPcp 0))) (G.Switch s2 :: path) topo p2)



module Gensym =
struct
  let count = ref 0
  let next () = incr count; !count
end

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

let rec to_dnf pol = 
  let pol' = to_dnf' pol in
  if dnf_form pol' then pol'
  else to_dnf pol'

let rec to_dnf_list pol = match pol with
  | RegUnion(a,b) -> to_dnf_list a @ to_dnf_list b
  | RegInter(a,b) -> [List.concat (to_dnf_list a @ to_dnf_list b) ]
  | RegPol(pr,re,k) -> [[(pr,re,k)]]

let rec dnf_form_pred pred = match pred with
  | And(Or _, _) -> false
  | And(_, Or _) -> false
  | Not (Not _) -> false
  | Not (And _) -> false
  | Not (Or _) -> false
  | And(a,b) -> dnf_form_pred a & dnf_form_pred b
  | Or(a,b) -> dnf_form_pred a & dnf_form_pred b
  | _ -> true

let rec to_dnf_pred' pred = match pred with
  | And (Or(a,b), c) -> Or(to_dnf_pred' (And(a,c)), to_dnf_pred' (And(b,c)))
  | And (c,Or(a,b)) -> Or(to_dnf_pred' (And(a,c)), to_dnf_pred' (And(b,c)))
  | And (a,b) -> And(to_dnf_pred' a, to_dnf_pred' b)
  | Or (a, b) -> Or( to_dnf_pred' a, to_dnf_pred' b)
  | _ -> pred

let rec demorganize pred = match pred with
  | Not (And(a,b)) -> Or (demorganize (Not a), demorganize (Not b))
  | Not (Or(a,b)) -> And (demorganize (Not a), demorganize (Not b))
  | Not (Not a) -> demorganize a
  | And(a,b) -> And (demorganize a, demorganize b)
  | Or(a,b) -> Or (demorganize a, demorganize b)
  | _ -> pred

let rec to_dnf_pred'' pred = 
  let pred' = to_dnf_pred' pred in
  if dnf_form_pred pred' then pred'
  else to_dnf_pred'' pred'

let rec to_dnf_pred pred = 
  to_dnf_pred'' (demorganize pred)

(* takes a DNF pred *)
let rec to_dnf_pred_list pred : predicate list list = match pred with
  | Or(a,b) -> to_dnf_pred_list a @ to_dnf_pred_list b
  | And(a,b) -> [List.concat (to_dnf_pred_list a @ to_dnf_pred_list b)]
  | _ -> [[pred]]

let trivial_pol = RegPol(NoPackets, Star, 0)

let rec product lst1 lst2 = match lst1 with
  | [] -> []
  | a :: lst1 -> (List.map (fun x -> (a,x)) lst2) @ product lst1 lst2

(* returns true if atomic pred pr2 is subsumed by atomic pred pr1 *)
let atom_matches pr1 pr2 = match pr1,pr2 with
  | All,_ -> true
  | _,NoPackets -> true
  | _ -> pr1 = pr2
  
(* pr2 is an atomic pred *)
let rec pred_matches pr1 pr2 =
  match pr1 with
    | All -> true
    | NoPackets -> false
    | And(a,b) -> pred_matches a pr2 & pred_matches b pr2
    | Or(a,b) -> pred_matches a pr2 or pred_matches b pr2
    | Not a -> not (pred_matches a pr2)
    | _ -> pr1 = pr2

(* thm: for phi,psi DNF, phi overlaps with psi iff one conjunction of phi overlaps with one conjunction of psi *)
(* DNF normal form: list of list of atoms *)
(* let rec preds_overlap pr1 pr2 = match pr1 with *)
(*   | All -> not (is_empty_pred pr2) *)
(*   | NoPackets -> false *)
(*   | Or(a,b) -> preds_overlap a pr2 or preds_overlap b pr2 *)
(*   | Not a -> not (preds_overlap a (Not pr2)) *)
(*   | And(a,b) -> preds_overlap a pr2 & preds_overlap b pr2 *)
(*   (\* atomic case *\) *)
(*   | _ -> pred_matches pr2 pr1 *)
(* and *)
(*     is_empty_pred pr = match pr with *)
(*       | NoPackets -> true *)
(*       | All -> false *)
(*       | DlType _ -> false *)
(*       | Or(a,b) -> is_empty_pred a & is_empty_pred b *)
(*       | Not a -> not (is_empty_pred a) *)
(*       | And(a,b) -> not (preds_overlap a b) *)

(* preds in DNF list form *)
let rec conj_overlap' pr1 pr2 =
  List.for_all (fun pr -> List.exists (atom_matches pr) pr2) pr1

(* preds in DNF list form *)
let rec conj_overlap pr1 pr2 = conj_overlap' pr1 pr2 or conj_overlap' pr2 pr1

(* preds in DNF list form *)
let rec preds_overlap pr1 pr2 = 
  (List.for_all (fun pr -> List.exists (conj_overlap pr) pr2) pr1) or
    (List.for_all (fun pr -> List.exists (conj_overlap pr) pr1) pr2)

let contradiction cnj = List.exists (fun pr -> not (conj_overlap [pr] cnj)) cnj

let is_empty dnf = List.for_all contradiction dnf

(* pr2 approx: may say no when equiv *)
let rec preds_equiv pr1 pr2 =
  pr1 = pr2

let print_list printer lst = 
  Printf.sprintf "[%s]" (String.concat ";" (List.map printer lst))

let rec blast_inter_list pol = 
  List.map (fun (a,b,c) -> RegPol(a,b,c)) 
    (List.map
       (fun pol -> (List.fold_left (fun (pr_a, re_a, k_a) (pr,re,k) ->
	 let pr1' = to_dnf_pred_list pr_a in
	 let pr2' = to_dnf_pred_list pr in
	 if preds_overlap pr1' pr2' 
	 then
	   (to_dnf_pred (And(pr_a, pr)), re_a && re, max k_a k)
	 else (NoPackets, Star, 0)) (All, Star, 0) pol)) pol)

let rec blast_inter pol = match pol with
  | RegInter(RegPol(pr1,re1,k1), RegPol(pr2,re2,k2)) ->
    let pr1' = to_dnf_pred_list pr1 in
    let pr2' = to_dnf_pred_list pr2 in
    if preds_overlap pr1' pr2' 
    then
      RegPol(And(pr1,pr2), re1 && re2, max k1 k2)
    else trivial_pol
  | RegInter(a, b) -> let RegPol(pr1, re1, k1) = blast_inter a in
		      let RegPol(pr2, re2, k2) = blast_inter b in
		      let pr1' = to_dnf_pred_list pr1 in
		      let pr2' = to_dnf_pred_list pr2 in
		      if preds_overlap pr1' pr2' 
		      then
			RegPol(And(pr1,pr2), re1 && re2, max k1 k2)
		      else trivial_pol

  | RegUnion(a, b) -> RegUnion(blast_inter a, blast_inter b)
  | RegPol _ -> pol

let rec blast_union1 pol1 pol2 = match pol1,pol2 with
  | RegPol(pr1,re1,k1), RegPol(pr2,re2,k2) ->
    let pr1' = to_dnf_pred_list pr1 in
    let pr2' = to_dnf_pred_list pr2 in
    match is_empty pr1', is_empty pr2' with
      | true,true -> []
      | true, false -> [pol2]
      | false, true -> [pol1]
      | _ ->
	if preds_equiv pr1 pr2 then
	  [RegPol(pr1, re1 && re2, max k1 k2)]
	else
	  if preds_overlap pr1' pr2' then
	    [RegPol(And (pr1, Not pr2), re1, k1);
	     RegPol(And (pr2, Not pr1), re2, k2);
	     RegPol(And (pr1, pr2), re1 && re2, max k1 k2)]
	  else
	    [RegPol(pr1,re1,k1); 
	     RegPol(pr2,re2,k2)]

let rec remove_dups' lst pol = match lst with
  | [] -> ([], pol)
  | RegPol(pr',re',k') :: lst -> 
    let RegPol(pr,re,k) = pol in
    if preds_equiv pr' pr then
      remove_dups' lst (RegPol(pr, re && re', max k k'))
    else
      let (rst, pol') = remove_dups' lst pol in
      (RegPol(pr',re',k') :: rst, pol')

let rec remove_dups pols = match pols with
  | [] -> []
  | p :: pols -> let rst,p' = remove_dups' pols p in
		 p' :: remove_dups rst
  
let rec blast_union pols = match pols with
  | [] -> []
  | pol :: pols -> List.concat (List.map (blast_union1 pol) pols) @ blast_union pols

(* let rec normalize pol =  *)
(*   blast_union (blast_inter (to_dnf pol)) *)

let rec simpl_pred pred = match pred with
  | Or(All, _) -> All
  | Or(_, All) -> All
  | Or(NoPackets, a) -> a
  | Or(a, NoPackets) -> a
  | Or (a,b) -> let a' = simpl_pred a in
		let b' = simpl_pred b in
		if preds_equiv a' b' then a' else Or (a', b')
  | And(All, a) -> a
  | And(a, All) -> a
  | And(NoPackets, _) -> NoPackets
  | And(_, NoPackets) -> NoPackets
  | And (a,b) -> let a' = simpl_pred a in
		 let b' = simpl_pred b in
		 if preds_equiv a' b' then a' else And (a', b')
  | Not (Not a) -> a
  | Not All -> NoPackets
  | Not NoPackets -> All
  | _ -> pred

let rec simpl_re re = match re with
  | Union(Star, _) -> Star
  | Union(_, Star) -> Star
  | Union(a, EmptySet) -> simpl_re a
  | Union(EmptySet, a) -> simpl_re a
  | Union(a,b) -> let a' = simpl_re a in
		  let b' = simpl_re b in
		  if a' = b' then a' else Union(a',b')
  | Intersection(Star, a) -> simpl_re a
  | Intersection(a, Star) -> simpl_re a
  | Intersection(EmptySet, a) -> EmptySet
  | Intersection(a, EmptySet) -> EmptySet
  | Intersection(a,b) -> let a' = simpl_re a in
			 let b' = simpl_re b in
			 if a' = b' then a' else Intersection(a',b')
  | Sequence(Empty,a) -> simpl_re a
  | Sequence(a,Empty) -> simpl_re a
  | Sequence(EmptySet, a) -> EmptySet
  | Sequence(a, EmptySet) -> EmptySet
  | Sequence(a,b) -> Sequence(simpl_re a, simpl_re b)
  | Comp Star -> EmptySet
  | Comp EmptySet -> Star
  | Comp (Comp a) -> simpl_re a
  | Comp a -> Comp (simpl_re a)
  | _ -> re

let simpl_pol = 
  List.map (fun pol ->
    let RegPol(pr,re,k) = pol in
    RegPol(simpl_pred pr, simpl_re re, k))

let remove_trivial = 
  List.fold_left (fun acc p -> 
    let RegPol(pr,re,k) = p in
    if pr = NoPackets or re = EmptySet then acc else p :: acc) []

(* iterate until the set of policies are disjoint. At each step,
   subtract off all the policies that do not overlap with any other
   policies, iterate over the remainder *)

let simplify pols =
  remove_dups (remove_trivial (simpl_pol pols))

(* atomic pols *)
let pols_disjoint pol1 pol2 = 
  let RegPol(pr1,_,_) = pol1 in
  let RegPol(pr2,_,_) = pol2 in
  let pr1' = to_dnf_pred_list (to_dnf_pred pr1) in
  let pr2' = to_dnf_pred_list (to_dnf_pred pr2) in
  not (preds_overlap pr1' pr2')

let rec are_disjoint pols = match pols with
  | [] -> true
  | pol :: pols -> List.for_all (pols_disjoint pol) pols & are_disjoint pols

let rec normalize' pols = 
  let pols' = simplify (blast_union (simplify pols))
  in
  if are_disjoint pols' then pols' else normalize' pols'

let normalize pol = 
  normalize' (blast_inter_list (to_dnf_list (to_dnf pol)))

let rec compile_regex pol topo = match pol with
  | RegPol (pred, reg, _) -> compile_path pred (expand_re reg topo) topo (Gensym.next ())
  | RegUnion (pol1, pol2) -> Par (compile_regex pol1 topo, compile_regex pol2 topo)
