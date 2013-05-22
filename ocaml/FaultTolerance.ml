open Pathetic.Regex
open Pathetic.RegexUtils
open OpenFlowTypes
open NetCoreFT

module G = Graph.Graph
module N = Graph

open NetCoreEval0x04

(** Constructing k-resilient trees (k-trees) from regular expression paths **)

let trivial_pol = Pol(NoPackets, [])

(* Tree w/ ordered children. Leafs are hosts, internal nodes are switches *)
type k_tree = 
  | KLeaf of N.node
  | KTree of N.node * (k_tree list)
  | KRoot of N.node * k_tree

exception NoTree of string
  
let rec k_tree_to_string tree = match tree with
  | KLeaf n -> 
    Printf.sprintf "KLeaf (%s)" (G.node_to_string n)
  | KTree(n, children) -> 
    Printf.sprintf "KTree(%s, [ %s ])" (G.node_to_string n) (String.concat "; " (List.map k_tree_to_string children))

let shortest_path_fail_set re sw topo fail_set =
  Printf.printf "[FaultTolerance.ml] shortest_path_fail_set %s %s %s\n" (regex_to_string re) (G.node_to_string sw)
    (String.concat ";" (List.map (fun (a,b) -> Printf.sprintf "(%s,%s)" (G.node_to_string a) (G.node_to_string b)) fail_set));
  let topo' = G.copy topo in
  G.del_links topo' fail_set;
  List.tl (shortest_path_re re sw topo')

(* Initial version: no backtracking *)
(* Build an (n - k) fault tolerant tree along 'path', avoiding links in 'fail_set' *)
let rec build_k_tree_from_path path regex n k fail_set topo = 
  Printf.printf "[FaultTolerance.ml] build_k_tree_from_path %s %s %d %d [%s]\n%!" (String.concat ";" (List.map G.node_to_string path)) (regex_to_string regex) n k (String.concat ";" (List.map (fun (a,b) -> Printf.sprintf "(%s,%s)" (G.node_to_string a) (G.node_to_string b)) fail_set));
  match path with
    | sw :: [ h ] -> Some (KTree(sw, [KLeaf h]))
    | N.Host h :: path -> 
      (match build_k_tree_from_path path (deriv (Const (N.Host h)) regex) n k fail_set topo with
	| None -> None
	| Some tree -> Some (KRoot (N.Host h, tree)))
    | sw :: path -> 
      (match build_k_children sw (deriv (Const sw) regex) n k fail_set topo with
	(* We haven't made any choices at this point, so we backtrack
	   up to our parent if we fail *)
	| None -> None
	| Some children -> Some (KTree(sw, children)))
and
    (* Build (n - k) backup paths at 'sw' according to 'regex', avoiding links in 'fail_set'. *)
    build_k_children sw regex n k fail_set topo =
  Printf.printf "[FaultTolerance.ml] build_k_children %s %d %d [%s]\n%!" (regex_to_string regex) n k (String.concat ";" (List.map (fun (a,b) -> Printf.sprintf "(%s,%s)" (G.node_to_string a) (G.node_to_string b)) fail_set));
  if k > n then Some [] 
  else
    let path = shortest_path_fail_set regex sw topo fail_set in
    match (List.hd path) with
      | N.Host h -> Some [KLeaf (N.Host h)]
      | new_sw' -> 
	(match build_k_tree_from_path path regex n k fail_set topo with
	  (* If we fail then we need to pick a new path *)
	  | None -> None
	  | Some tree -> (match build_k_children sw regex n (k + 1) ((sw, new_sw') :: fail_set) topo with
	      (* If we fail here, either because we chose a bad ordering, or because we chose a bad path earlier *)
	      | None -> None
	      | Some children -> Some (tree :: children)))


let build_k_tree n regex topo = 
  let path = expand_re regex topo in
  match List.hd path with
    | N.Host h ->
      (match build_k_tree_from_path path regex n 0 [] topo with
	| None -> raise (NoTree "failed to build k-tree")
	| Some tree -> tree)


(** Compiling k-trees into NetCore policies **)

let strip_tag = { unmodified with modifyDlVlan = Some None }

let stamp_path_tag pathTag tag = 
  (* If we set the VLAN, the controller will push a new tag on. Should probably fix that *)
  { unmodified with modifyDlVlan = Some (Some pathTag); modifyDlVlanPcp = (Some tag) }


let stamp_tag tag = 
  (* If we set the VLAN, the controller will push a new tag on. Should probably fix that *)
  { unmodified with modifyDlVlanPcp = (Some tag) }

let match_tag pathTag tag = 
  And( DlVlan (Some pathTag), DlVlanPcp tag )

module GenSym =
struct
  let create () = ref 0
  let next_val g =  incr g; !g
end

type tagged_k_tree = 
  | KLeaf_t of N.node
  | KTree_t of N.node * ((int * tagged_k_tree) list)
  | KRoot_t of N.node * tagged_k_tree

let rec tagged_k_tree_to_string tree = match tree with
  | KLeaf_t h -> Printf.sprintf "KLeaf_t %s" (G.node_to_string h)
  | KTree_t(sw, children) -> Printf.sprintf "KTree(%s, [ %s ])" (G.node_to_string sw)
    (String.concat "; " (List.map 
			   (fun (k,t) -> Printf.sprintf "(%d,%s)" k (tagged_k_tree_to_string t)) 
			   children))
  | KRoot_t(h, tree) -> Printf.sprintf "KRoot_t %s (%s)" (G.node_to_string h) (tagged_k_tree_to_string tree)

let rec tag_k_tree tree tag gensym = match tree with
  | KLeaf h -> KLeaf_t h
  | KTree (sw, (first :: rest)) -> 
    (* First child gets the parent's tag *)
    let first_child = (tag, tag_k_tree first tag gensym) in
    (* Other children get unique genSym'ed tag *)
    let backup_children = List.map (fun child -> let new_tag = (GenSym.next_val gensym) in
						 (new_tag, tag_k_tree child new_tag gensym)) rest in
    KTree_t (sw, first_child :: backup_children)
  | KRoot (h, tree) -> KRoot_t(h, tag_k_tree tree tag gensym)

let next_port_from_k_tree sw topo pathTag tree = 
  (* Printf.printf "[FaulTolerance.ml] next_hop_from_k_tree %s\n%!" (k_tree_to_string tree); *)
  match tree with
  | (tag, KLeaf_t host) -> 
    let (_,p1) = G.get_ports topo host sw in
    To(strip_tag, p1)
  | (tag, KTree_t (sw', _)) -> 
    let p1,p2 = G.get_ports topo sw sw' in
    To(stamp_tag tag, p1)

let next_hop_from_k_tree sw topo tree = 
  (* Printf.printf "[FaulTolerance.ml] next_hop_from_k_tree %s\n%!" (k_tree_to_string tree); *)
  match tree with
  | (_, KLeaf_t host) -> 
    let (p1,p2) = G.get_ports topo host sw in
    (sw, p1, tree)
  | (_, KTree_t (sw', _)) -> 
    let p1,p2 = G.get_ports topo sw sw' in
    (sw', p2, tree)

(* Converts a k fault tolerant tree into a NetCore policy *)
let rec policy_from_k_tree' inport tree topo path_tag tag = 
  Printf.printf "[FaulTolerance.ml] policy_from_k_tree' %ld %s\n%!" inport (tagged_k_tree_to_string tree);
  match tree with
    | KLeaf_t h -> 
      trivial_pol
    | KTree_t(sw', children) -> 
      let N.Switch sw = sw' in
      let children_actions = List.map (next_port_from_k_tree sw' topo path_tag) children in
      let backup = LPar(And( Switch sw, And( InPort inport, match_tag path_tag tag )), 
			children_actions) in
      let next_hops = List.map (next_hop_from_k_tree sw' topo) children in
      let children_pols = List.fold_left 
	(fun a (sw'', inport,tree) -> 
	  Par(a, policy_from_k_tree' inport (snd tree) topo path_tag (fst tree))) trivial_pol next_hops in
      Par(backup, children_pols)

let next_port_from_k_tree_root sw topo pathTag tree = 
  match tree with
  | (tag, KLeaf_t host) -> 
    let (_,p1) = G.get_ports topo host sw in
    To(strip_tag, p1)
  | (tag, KTree_t (sw', _)) -> 
    let p1,p2 = G.get_ports topo sw sw' in
    To(stamp_path_tag pathTag tag, p1)

let policy_from_k_tree pr tree topo path_tag tag =  
  Printf.printf "[FaulTolerance.ml] policy_from_k_tree %s\n%!" (tagged_k_tree_to_string tree);
  match tree with
    | KRoot_t(N.Host h, KTree_t(N.Switch sw, children)) -> 
      let sw' = N.Switch sw in
      let _,inport = G.get_ports topo (N.Host h) sw' in
      let children_ports = List.map (next_port_from_k_tree_root sw' topo path_tag) children in
      let backup = LPar(And( Switch sw, And( InPort inport, pr)), 
			children_ports) in
      let next_hops = List.map (next_hop_from_k_tree (N.Switch sw) topo) children in
      let children_pols = List.fold_left 
	(fun a (sw'', inport,tree) -> 
	  Par(a, policy_from_k_tree' inport (snd tree) topo path_tag (fst tree))) trivial_pol next_hops in
      Par(backup, children_pols)


let rec compile_ft_regex pred vid (regex : regex) k topo = 
  let ktree = build_k_tree k regex topo in
  let genSym = GenSym.create() in
  let tag = GenSym.next_val genSym in
  let tagged_ktree = tag_k_tree ktree tag genSym in
  policy_from_k_tree pred tagged_ktree topo vid tag

    
let rec compile_ft_to_nc1 regpol topo genSym =
  match regpol with
    | RegUnion (p1,p2) -> Par(compile_ft_to_nc1 p1 topo genSym, compile_ft_to_nc1 p2 topo genSym)
    | RegPol (pred, path, k) -> let vid = GenSym.next_val genSym in
				      compile_ft_regex pred vid path k topo

let rec compile_ft_to_nc regpol topo =
  compile_ft_to_nc1 regpol topo (GenSym.create())
