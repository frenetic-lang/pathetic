open Pathetic.Regex
open OpenFlowTypes
open NetCoreFT

module G = Graph.Graph

open NetCoreEval0x04

(** Constructing k-resilient trees (k-trees) from regular expression paths **)

let trivial_pol = Pol(NoPackets, [])

(* Tree w/ ordered children. Leafs are hosts, internal nodes are switches *)
type k_tree = 
  | KLeaf of int
  | KTree of switchId * (k_tree list)

exception NoTree of string

let rec k_tree_to_string tree = match tree with
  | KLeaf h -> Printf.sprintf "KLeaf %d" h
  | KTree(sw, children) -> Printf.sprintf "KTree(%Ld, [ %s ])" sw (String.concat "; " (List.map k_tree_to_string children))

(* Takes a path-regex and deletes the expanded path, replacing it with the original regex *)
let clear_path path = List.map (fun a -> (a,a)) (Pathetic.Regex.collapse_star (List.map snd path))

(* Initial version: no backtracking *)
(* Build an (n - k) fault tolerant tree along 'path', avoiding links in 'fail_set' *)
(* back-tracking invariant: returns None if there is no n-k FT tree along 'path' *)
let rec build_k_tree_from_path path regex n k fail_set topo = 
  Printf.printf "[FaultTolerance.ml] build_k_tree_from_path %s\n%!" 
    (String.concat ";" (List.map regex_to_string path));
  match path with
    | Hop sw :: [ Host h1 ] -> Some (KTree(sw, [KLeaf h1]))
    | Hop sw :: Hop sw' :: path -> 
      (match build_k_children sw (List.tl regex) n k fail_set topo with
	(* We haven't made any choices at this point, so we backtrack
	   up to our parent if we fail *)
	| None -> None
	| Some children -> Some (KTree(sw, children)))
    | Host h :: path -> build_k_tree_from_path path (List.tl regex) n k fail_set topo
and
    (* Build (n - k) backup paths at 'sw' according to 'regex', avoiding links in 'fail_set'. *)
    (* backtracking invariant: returns None iff there are not n-k FT backup paths at this node *)
    build_k_children sw regex n k fail_set topo =
  if k > n then Some [] 
  else
    let path, regex = expand_path_with_match_bad_links regex sw topo fail_set in
    match List.hd path with
      | Host h -> Some [KLeaf h]
      | Hop new_sw' -> 
	(match build_k_tree_from_path path regex n k fail_set topo with
	  (* If we fail then we need to pick a new path *)
	  | None -> None
	  | Some tree -> (match build_k_children sw regex n (k + 1) ((sw, new_sw') :: fail_set) topo with
	      (* If we fail here, either because we chose a bad ordering, or because we chose a bad path earlier *)
	      | None -> None
	      | Some children -> Some (tree :: children)))


let build_k_tree n regex topo = 
  let path, regex = expand_path_with_match regex topo in
  match build_k_tree_from_path path regex n 0 [] topo with
    | None -> raise (NoTree "failed to build k-tree")
    | Some tree -> tree

let strip_tag = { unmodified with modifyDlVlan = Some None }

let stamp_tag flag pathTag tag = 
  (* If we set the VLAN, the controller will push a new tag on. Should probably fix that *)
  if flag then
    { unmodified with modifyDlVlan = Some (Some pathTag); modifyDlVlanPcp = (Some tag) }
  else
    { unmodified with modifyDlVlanPcp = (Some tag) }

let match_tag flag pathTag tag = 
  if flag then
    All
  else
    And( DlVlan (Some pathTag), DlVlanPcp tag )

(** Compiling k-trees into NetCore policies **)

module GenSym =
struct
  let create () = ref 0
  let next_val g =  incr g; !g
end

type tagged_k_tree = 
  | KLeaf_t of int
  | KTree_t of switchId * ((int * tagged_k_tree) list)

let rec tagged_k_tree_to_string tree = match tree with
  | KLeaf_t h -> Printf.sprintf "KLeaf_t %d" h
  | KTree_t(sw, children) -> Printf.sprintf "KTree(%Ld, [ %s ])" sw 
    (String.concat "; " (List.map 
			   (fun (k,t) -> Printf.sprintf "(%d,%s)" k (tagged_k_tree_to_string t)) 
			   children))

let rec tag_k_tree tree tag gensym = match tree with
  | KLeaf h -> KLeaf_t h
  | KTree (sw, (first :: rest)) -> 
    (* First child gets the parent's tag *)
    let first_child = (tag, tag_k_tree first tag gensym) in
    (* Other children get unique genSym'ed tag *)
    let backup_children = List.map (fun child -> let new_tag = (GenSym.next_val gensym) in
						 (new_tag, tag_k_tree child new_tag gensym)) rest in
    KTree_t (sw, first_child :: backup_children)

let next_port_from_k_tree sw topo first_hop_flag pathTag tree = 
  (* Printf.printf "[FaulTolerance.ml] next_hop_from_k_tree %s\n%!" (k_tree_to_string tree); *)
  match tree with
  | (tag, KLeaf_t host) -> 
    let Some (s1,p1) = G.get_host_port topo host in
    assert (s1 = sw); To(strip_tag, p1)
  | (tag, KTree_t (sw', _)) -> 
    let p1,p2 = G.get_ports topo sw sw' in
    To(stamp_tag first_hop_flag pathTag tag, p1)

let next_hop_from_k_tree sw topo tree = 
  (* Printf.printf "[FaulTolerance.ml] next_hop_from_k_tree %s\n%!" (k_tree_to_string tree); *)
  match tree with
  | (_, KLeaf_t host) -> 
    let Some (s1,p1) = G.get_host_port topo host in
    assert (s1 = sw); (sw, p1, tree)
  | (_, KTree_t (sw', _)) -> 
    let p1,p2 = G.get_ports topo sw sw' in
    (sw', p2, tree)

(* Converts a k fault tolerant tree into a NetCore policy *)
let rec policy_from_k_tree pr sw inport first_hop_flag tree topo path_tag tag = 
  Printf.printf "[FaulTolerance.ml] policy_from_k_tree %Ld %s\n%!" sw (tagged_k_tree_to_string tree);
  match tree with
    | KLeaf_t h -> let Some (s1,p1) = G.get_host_port topo h in
		   assert (s1 = sw); 
		   Pol(And( Switch sw, And(InPort inport, And(pr, match_tag first_hop_flag path_tag tag))), 
		       [To(strip_tag, p1)])
    | KTree_t(sw', children) -> 
      assert (sw = sw');
      let children_ports = List.map (next_port_from_k_tree sw' topo first_hop_flag path_tag) children in
      let backup = LPar(And( Switch sw', And( InPort inport, And( pr, match_tag first_hop_flag path_tag tag ))), 
			children_ports) in
      let next_hops = List.map (next_hop_from_k_tree sw' topo) children in
      let children_pols = List.fold_left 
	(fun a (sw'', inport,tree) -> 
	  Par(a, policy_from_k_tree pr sw'' inport false (snd tree) topo path_tag (fst tree))) trivial_pol next_hops in
      Par(backup, children_pols)

let rec compile_ft_regex pred vid regex k topo = 
  let Host srcHost = List.hd regex in
  let ktree = build_k_tree k regex topo in
  let Some (srcSw,srcPort) = G.get_host_port topo srcHost in
  let genSym = GenSym.create() in
  let tag = GenSym.next_val genSym in
  let tagged_ktree = tag_k_tree ktree tag genSym in
  policy_from_k_tree pred srcSw srcPort true tagged_ktree topo vid tag

    
let rec compile_ft_to_nc1 regpol topo genSym =
  match regpol with
    | RegUnion (p1,p2) -> Par(compile_ft_to_nc1 p1 topo genSym, compile_ft_to_nc1 p2 topo genSym)
    | RegPol (pred, path, k) -> let vid = GenSym.next_val genSym in
				      compile_ft_regex pred vid (flatten_reg path) k topo

let rec compile_ft_to_nc regpol topo =
  compile_ft_to_nc1 regpol topo (GenSym.create())
