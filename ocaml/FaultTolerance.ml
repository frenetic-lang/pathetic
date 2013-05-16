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
(* Build (n - k) backup paths at 'sw' according to spec 'path' avoiding links 'fail_set'. *)
let rec build_k_children sw path n k fail_set topo =
  if k > n then Some [] 
  else
    let path = expand_path_with_match_bad_links path sw topo fail_set in
    match List.hd path with
      | (Host h,_) -> Some [KLeaf h]
      | (Hop new_sw',_) -> 
	(match build_k_tree_from_path path n k fail_set topo with
	  | None -> None
	  | Some tree -> (match build_k_children sw (clear_path path) n (k + 1) ((sw, new_sw') :: fail_set) topo with
	      | None -> None
	      | Some children -> Some (tree :: children)))
and
    (* Build an (n - k) fault tolerant tree along 'path', avoiding links in 'fail_set' *)
    build_k_tree_from_path path n k fail_set topo = 
      Printf.printf "[FaultTolerance.ml] build_k_tree_from_path %s\n%!" 
	(String.concat ";" (List.map (fun (a,b) -> 
	  Printf.sprintf "(%s, %s)" (regex_to_string a) (regex_to_string b)) path));
    match path with
      | (Hop sw, _) :: [(Host h1,_)] -> Some (KTree(sw, [KLeaf h1]))
      | (Hop sw, a) :: (Hop sw', b) :: path -> 
	(match build_k_children sw (clear_path ((b, b) :: path)) n k fail_set topo with
	  | None -> None
	  | Some children -> Some (KTree(sw, children)))
      | (Host h, _) :: path -> build_k_tree_from_path path n k fail_set topo

let build_k_tree n regex topo = 
  match build_k_tree_from_path (expand_path_with_match regex topo) n 0 [] topo with
    | None -> raise (NoTree "failed to build k-tree")
    | Some tree -> tree

let strip_tag = {unmodified with modifyDlVlan = Some None}

let stamp_tag flag pathTag tag = 
  (* If we set the VLAN, the controller will push a new tag on. Should probably fix that *)
  if flag then
    {unmodified with modifyDlVlan = Some (Some pathTag); modifyDlVlanPcp = (Some tag)}
  else
    {unmodified with modifyDlVlanPcp = (Some tag)}

let match_tag flag pathTag tag = 
  if flag then
    All
  else
    And(DlVlan (Some pathTag), DlVlanPcp tag)

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

let next_port_from_k_tree pr sw topo first_hop_flag pathTag tree = 
  (* Printf.printf "[FaulTolerance.ml] next_hop_from_k_tree %s\n%!" (k_tree_to_string tree); *)
  match tree with
  | (tag, KLeaf_t host) -> (match G.get_host_port topo host with
      | Some (s1,p1) -> 
	assert (s1 = sw); To(strip_tag, p1))
  | (tag, KTree_t (sw', _)) -> 
    (match G.get_ports topo sw sw' with
      | (p1,p2) -> To(stamp_tag first_hop_flag pathTag tag, p1))

let next_hop_from_k_tree pr sw topo pathTag tree = 
  (* Printf.printf "[FaulTolerance.ml] next_hop_from_k_tree %s\n%!" (k_tree_to_string tree); *)
  match tree with
  | (tag, KLeaf_t host) -> (match G.get_host_port topo host with
      | Some (s1,p1) -> 
	assert (s1 = sw); (sw, p1, tree))
  | (tag, KTree_t (sw', _)) -> 
    (match G.get_ports topo sw sw' with
      | (p1,p2) -> (sw', p2, tree))

(* Converts a k fault tolerant tree into a NetCore policy *)
let rec policy_from_k_tree pr sw inport first_hop_flag tree topo pathTag tag = 
  Printf.printf "[FaulTolerance.ml] policy_from_k_tree %Ld %s\n%!" sw (tagged_k_tree_to_string tree);
  match tree with
    | KLeaf_t h -> (match G.get_host_port topo h with
	| Some (s1,p1) -> assert (s1 = sw); 
	  Pol(And( Switch sw, And(InPort inport, And(pr, match_tag first_hop_flag pathTag tag))), 
	      [To(strip_tag, p1)]))
    | KTree_t(sw', children) -> 
      assert (sw = sw');
      let children_ports = List.map (next_port_from_k_tree pr sw' topo first_hop_flag pathTag) children in
      let backup = LPar(And( Switch sw', And (InPort inport, And(pr, match_tag first_hop_flag pathTag tag))), 
			children_ports) in
      let next_hops = List.map (next_hop_from_k_tree pr sw' topo pathTag) children in
      let children_pols = List.fold_left 
	(fun a (sw'', inport,tree) -> 
	  Par(a, policy_from_k_tree pr sw'' inport false (snd tree) topo pathTag (fst tree))) trivial_pol next_hops in
      Par(backup, children_pols)

let rec compile_ft_regex pred vid regex k topo = 
  let Host srcHost = List.hd regex in
  let ktree = build_k_tree k regex topo in
  let srcSw,srcPort = (match G.get_host_port topo srcHost with Some (sw,p) -> (sw,p)) in
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
