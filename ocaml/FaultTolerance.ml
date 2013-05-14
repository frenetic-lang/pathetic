open Pathetic.Regex
open OpenFlowTypes
open NetCoreFT

module G = Graph.Graph
module Q = Queue
module H = Hashtbl

let rec get_ports acts = match acts with
  | (To (_,p)) :: acts -> Some p :: get_ports acts
  | ToAll _ :: acts -> None :: get_ports acts
  | _ :: acts -> get_ports acts
  | [] -> []

let rec get_switches pred topo switches = match pred with
  | And (p1,p2) -> Graph.SwSet.inter (get_switches p1 topo switches) (get_switches p2 topo switches)
  | Or (p1,p2) -> Graph.SwSet.union (get_switches p1 topo switches) (get_switches p2 topo switches)
  | Not p1 -> Graph.SwSet.diff switches (get_switches p1 topo switches)
  | All -> switches
  | Switch sw -> Graph.SwSet.singleton sw
  | _ -> Graph.SwSet.empty

let rec get_links switches ports topo = match switches with
  | [] -> []
  | sw :: switches -> (List.map (fun p -> (sw,p)) ports) @ get_links switches ports topo

let normalize_link sw1 p1 sw2 p2 = 
  if sw1 > sw2 then ((sw1, p1), (sw2, p2))
  else if sw2 > sw1 then ((sw2, p2), (sw1, p1))
  else if p1 > p2 then ((sw1, p1), (sw2, p2))
  else ((sw2, p2), (sw1, p1))

(*
  Given a netcore policy and a topology, we can compute the
  edges/switches that policy 'depends' upon. When a needed link goes
  down, the policy has failed
*)

let rec dependent_links pol topo = match pol with
  | Par (p1, p2) -> (dependent_links p1 topo) @ (dependent_links p2 topo)
  | Pol (pred, acts) -> get_links (Graph.SwSet.elements (get_switches pred topo (G.nodes topo))) (get_ports acts) topo

(* Todo: given a list of NC policies, determine the minimum number of link failures to cause the policies to fail *)

(* 
   Let's simplify and assume that we have a single policy for a single
   flow. We represent the policies as a mapping from switches to ports,
   specifying the next hop from that switch 
*)
(* 
   Algorithm: BFS across the topology. At each node, calculate the
   minimum number of failures required to prevent forwarding. This number
   is the sum of the minimum number of failures required to reach this
   node + min number of port failures such that no policies are
   available 
*)

let rec from lst n = match (n, lst) with
  | 0,_ -> lst
  | n,[] -> raise (Invalid_argument (Pervasives.string_of_int n))
  | n, l::lst -> from lst (n - 1)

let min_num_port_failures sw pols = 0

(* Stupid Ubuntu doesn't have OCaml 4, so List.mapi doesn't exist *)

let rec mapi' idx f lst =
  match lst with
    | [] -> []
    | l :: lst -> (f l idx) :: mapi' (idx + 1) f lst

let mapi f lst = mapi' 0 f lst

(* First pass computes the minimum number of failures required to reach each switch *)
let rec min_failures topo policies queue arr = 
  if Q.is_empty queue then arr
  else
    let (sw, current) = (Q.take queue) in
    let best = try (Pervasives.min (H.find arr sw) current) with _ -> current in
    if current >= best then arr else
      let nbrs = mapi (fun p count -> (G.next_hop topo sw p, count)) (from (H.find policies sw) best) in
      let () = H.add arr sw best;
	List.iter (fun (sw', count) -> Q.add (sw', best + count) queue) nbrs in
      min_failures topo policies queue arr

(* Second pass computes the minimum number of failures at a switch to prevent forwarding *)

module PtSet = Set.Make(
  struct
    let compare = Pervasives.compare
    type t = G.b
  end)

let list_to_set lst =
  List.fold_left (fun acc sw -> PtSet.add sw acc) PtSet.empty lst

let rec min_port_failures switches policies arr = 
  match switches with 
    | [] -> arr
    | sw :: switches -> let () = H.add arr sw (PtSet.cardinal (list_to_set (H.find policies sw))) in
			min_port_failures switches policies arr

(* Third pass computes the sum of pass 1 and 2 and returns the min over all switches *)
let fault_tolerance topo policies = 
  let switches = (Graph.SwSet.elements (G.nodes topo)) in
  let arr1 = min_failures topo policies (Q.create ()) (H.create 5) in
  let arr2 = min_port_failures switches policies (H.create 5) in
  List.fold_left (fun acc sw -> Pervasives.min acc ((H.find arr1 sw) + (H.find arr2 sw))) Pervasives.max_int switches

(* Fault tolerance analysis implemented over a directed, rooted DAG *)

module Gensym =
struct
  let count = ref (Int32.of_int 0)
  let next () = count := Int32.succ !count; !count
end

let groups_to_string groups =
  String.concat ";\n" (List.map (fun (gid,_,acts) -> Printf.sprintf "\t%ld" gid) groups)

let add_group groups gid acts =
  groups := (gid, FF, acts) :: !groups

open NetCoreEval0x04

let rec from n lst = match lst with
  | [] -> [] (* Throw error? *)
  | l :: lst -> if n <= 0 then l::lst
    else from (n - 1) lst

module Dag =
  struct
    type a = switchId
    type b = portId
    type dag = ((a, (int, b) H.t) H.t) * G.graph
    let install_link (d, g) topo sw sw' k =
      let p1,p2 = G.get_ports topo sw sw' in
      G.add_edge g sw p1 sw' p2;
      let portTbl = (try H.find d sw with _ -> H.create 5) in
      H.add portTbl k p1;
      H.add d sw portTbl

    let install_host_link (d, g) sw port k =
      let portTbl = (try H.find d sw with _ -> H.create 5) in
      H.add portTbl k port;
      H.add d sw portTbl

    let create () : dag = (H.create 5, G.create ())
    let rec insert i b lst = match lst with
      | (j, c) :: lst -> if j < i then
	  (j,c) :: insert i b lst
	else (i,b) :: (j,c) :: lst
      | [] -> [(i,b)]

    let next_hops (d,_) sw = 
      try snd (List.split (H.fold (fun idx port acc -> insert idx port acc) (H.find d sw) [])) with _ -> []
    let next_hop (_,g) sw p =
      let sw' = G.next_hop g sw p in
      let _,p' = G.get_ports g sw sw' in
      (sw',p')
    let intTbl_to_string intTbl =
      String.concat "" (H.fold (fun k p acc -> (Printf.sprintf "\t\t%d -> %ld\n" k p):: acc) intTbl [])
    let dag_to_string (d,_) = String.concat "" (H.fold (fun sw intTbl acc -> (Printf.sprintf "\t%Ld -> \n%s\n" sw (intTbl_to_string intTbl)):: acc) d [])
  end

let expand_regex_with_match_bad_links regex sw topo bad_links = expand_path_with_match1 regex sw topo

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
let rec build_k_children sw path n k fail_set topo =
  if k > n then Some [] 
  else
    let path = expand_regex_with_match_bad_links path sw topo fail_set in
    match (List.hd (fst (List.split path))) with
      | Host h -> Some [KLeaf h]
      | Hop new_sw' -> 
	(match build_k_tree_from_path path n k fail_set topo with
	  | None -> None
	  | Some tree -> (match build_k_children sw path n (k + 1) ((sw, new_sw') :: fail_set) topo with
	      | None -> None
	      | Some children -> Some (tree :: children)))
and
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
  let create () = ref 0
  let next_val g = let v = !g in
		   incr g;
		   v
end

let next_hop_from_k_tree pr sw tree topo pathTag tag = 
  Printf.printf "[FaulTolerance.ml] next_hop_from_k_tree %s\n%!" (k_tree_to_string tree);
  match tree with
  | KLeaf host -> (match G.get_host_port topo host with
      | Some (s1,p1) -> 
	Printf.printf "[FaulTolerance.ml] next_hop_from_k_tree %Ld %Ld\n%!" sw s1;
	assert (s1 = sw); (To(strip_tag, p1)), sw, p1, tree)
  | KTree (sw', _) -> (match G.get_ports topo sw sw' with
      | (p1,p2) -> (To(stamp_tag pathTag tag, p1)), sw', p2, tree)

(* Need to add inport matching *)
let rec policy_from_k_tree pr sw inport tree topo pathTag tag gensym = 
  Printf.printf "[FaulTolerance.ml] policy_from_k_tree %Ld %s\n%!" sw (k_tree_to_string tree);
  match tree with
    | KLeaf h -> (match G.get_host_port topo h with
	| Some (s1,p1) -> assert (s1 = sw); Pol(And( Switch sw, And(InPort inport, And(pr, match_tag pathTag tag))), [To(strip_tag, p1)]))
    | KTree(sw', children) -> 
      assert (sw = sw');
      let children_ports = List.map (fun b -> next_hop_from_k_tree pr sw' b topo pathTag (Gen.next_val gensym)) children in
      let backup = LPar(And( Switch sw', And (InPort inport, And(pr, match_tag pathTag tag))), List.map (fun (a,b,c,d) -> a) children_ports) in
      let children_pols = List.fold_left (fun a (_,sw'', inport,tree) -> Par(a, policy_from_k_tree pr sw'' inport tree topo pathTag (Gen.next_val gensym) gensym)) trivial_pol children_ports in
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
  policy_from_k_tree pred srcSw srcPort ktree topo vid (Gen.next_val genSym) genSym

    
let rec compile_ft_to_nc regpol topo =
  match regpol with
    | RegUnion (p1,p2) -> Par(compile_ft_to_nc p1 topo, compile_ft_to_nc p2 topo)
    | RegPol (pred, path, k) -> let vid = Int32.to_int (Gensym.next ()) in
				      compile_ft_regex pred vid (flatten_reg path) k topo
