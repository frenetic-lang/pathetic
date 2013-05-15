open Pathetic.Regex
open OpenFlowTypes
open NetCoreFT

module G = Graph.Graph

module Gensym =
struct
  let count = ref (Int32.of_int 0)
  let next () = count := Int32.succ !count; !count
end

open NetCoreEval0x04

let rec range k n = if k = n then [] else k :: range (k+1) n

let trivial_pol = Pol(NoPackets, [])

type k_tree = 
  | KLeaf of int
  | KTree of switchId * (k_tree list)

let rec k_tree_to_string tree = match tree with
  | KLeaf h -> Printf.sprintf "KLeaf %d" h
  | KTree(sw, children) -> Printf.sprintf "KTree(%Ld, [ %s ])" sw (String.concat "; " (List.map k_tree_to_string children))

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
      Printf.printf "[FaultTolerance.ml] build_k_tree_from_path %s\n%!" (String.concat ";" (List.map (fun (a,b) -> 
      Printf.sprintf "(%s, %s)" (regex_to_string a) (regex_to_string b)) path));
    match path with
      | (Hop sw, _) :: [(Host h1,_)] -> Some (KTree(sw, [KLeaf h1]))
      | (Hop sw, a) :: (Hop sw', b) :: path -> 
	(match build_k_children sw (clear_path ((b, b) :: path)) n k fail_set topo with
	  | None -> None
	  | Some children -> Some (KTree(sw, children)))
      | (Host h, _) :: path -> build_k_tree_from_path path n k fail_set topo

exception NoTree of string

let build_k_tree n regex topo = 
  let path = expand_path_with_match regex topo in
  match build_k_tree_from_path path n 0 [] topo with
    | None -> raise (NoTree "failed to build k-tree")
    | Some tree -> tree

let strip_tag = {unmodified with modifyDlVlan = Some None}
let stamp_tag pathTag tag = {unmodified with modifyDlVlan = Some (Some pathTag); modifyDlVlanPcp = (Some tag)}
let match_tag pathTag tag = And(DlVlan (Some pathTag), DlVlanPcp tag)

module Gen =
struct
  let create () = ref 1
  let next_val g = let v = !g in
		   incr g;
		   v
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
  | KTree (sw, children) -> let first_child = (tag, tag_k_tree (List.hd children) tag gensym) in
			    let backup_children = List.map (fun child -> let new_tag = (Gen.next_val gensym) in
									 (new_tag, tag_k_tree child new_tag gensym)) (List.tl children) in
			    KTree_t (sw, first_child :: backup_children)

let next_hop_from_k_tree pr sw tree topo pathTag = 
  (* Printf.printf "[FaulTolerance.ml] next_hop_from_k_tree %s\n%!" (k_tree_to_string tree); *)
  match tree with
  | (tag, KLeaf_t host) -> (match G.get_host_port topo host with
      | Some (s1,p1) -> 
	Printf.printf "[FaulTolerance.ml] next_hop_from_k_tree %Ld %Ld\n%!" sw s1;
	assert (s1 = sw); (To(strip_tag, p1)), sw, p1, tree)
  | (tag, KTree_t (sw', _)) -> (match G.get_ports topo sw sw' with
      | (p1,p2) -> (To(stamp_tag pathTag tag, p1)), sw', p2, tree)

let rec range fst lst = 
  if fst >= lst then [] else
    fst :: range (fst + 1) lst

(* Need to add inport matching *)
let rec policy_from_k_tree pr sw inport flag tree topo pathTag tag = 
  Printf.printf "[FaulTolerance.ml] policy_from_k_tree %Ld %s\n%!" sw (tagged_k_tree_to_string tree);
  match tree with
    | KLeaf_t h -> (match G.get_host_port topo h with
	| Some (s1,p1) -> assert (s1 = sw); Pol(And( Switch sw, And(InPort inport, And(pr, match_tag pathTag tag))), [To(strip_tag, p1)]))
    | KTree_t(sw', children) -> 
      assert (sw = sw');
      let children_ports = List.map (fun a -> next_hop_from_k_tree pr sw' a topo pathTag) children in
      let backup = LPar(And( Switch sw', And (InPort inport, And(pr, (if flag then All else match_tag pathTag tag)))), List.map (fun (a,b,c,d) -> a) children_ports) in
      let children_pols = List.fold_left (fun a (_,sw'', inport,tree) -> Par(a, policy_from_k_tree pr sw'' inport false (snd tree) topo pathTag (fst tree))) trivial_pol children_ports in
      Par(backup, children_pols)

let first = List.hd
let rec last lst = 
  match lst with
    | [l] -> l
    | a :: lst -> last lst

let rec compile_ft_regex pred vid regex k topo = 
  let Host srcHost = first regex in
  let Host dstHost = last regex in
  let ktree = build_k_tree k regex topo in
  let srcSw,srcPort = (match G.get_host_port topo srcHost with Some (sw,p) -> (sw,p)) in
  let dstSw,dstPort = (match G.get_host_port topo dstHost with Some (sw,p) -> (sw,p)) in
  let genSym = Gen.create() in
  let tag = Gen.next_val genSym in
  let tagged_ktree = tag_k_tree ktree tag genSym in
  policy_from_k_tree pred srcSw srcPort true tagged_ktree topo vid tag

    
let rec compile_ft_to_nc regpol topo =
  match regpol with
    | RegUnion (p1,p2) -> Par(compile_ft_to_nc p1 topo, compile_ft_to_nc p2 topo)
    | RegPol (pred, path, k) -> let vid = Int32.to_int (Gensym.next ()) in
				      compile_ft_regex pred vid (flatten_reg path) k topo
