open OpenFlow0x01Types
open NetCoreFT

module G = Graph.Graph

(* type graph = (switchId * switchId * int) list *)


type regex =
  | Hop of switchId
  | Host of int
  | Star
  | Sequence of regex * regex
  | Union of regex * regex
  | Intersection of regex * regex

type regex_policy = 
  | RegPol of predicate * regex * int
  | RegUnion of regex_policy * regex_policy
  | RegInter of regex_policy * regex_policy

let (<+>) a b = RegUnion(a,b)
let (<*>) a b = RegInter(a,b)

let (&&) a b = Intersection(a,b)
let (||) a b = Union(a,b)
let (<.>) a b = Sequence(a,b)

let rec regex_to_string reg = match reg with
  | Hop sw -> Printf.sprintf "(Hop %Ld)" sw
  | Host h -> Printf.sprintf "(Host %d)" h
  | Star -> "*"
  | Sequence(reg1, reg2) -> Printf.sprintf "( %s <.> %s )" (regex_to_string reg1) (regex_to_string reg2)
  | Union(reg1, reg2) -> Printf.sprintf "( %s <||> %s )" (regex_to_string reg1) (regex_to_string reg2)
  | Intersection(reg1, reg2) -> Printf.sprintf "( %s <&&> %s )" (regex_to_string reg1) (regex_to_string reg2)

let rec regexPol_to_string regPol = match regPol with
  | RegPol(pr, reg, k) -> Printf.sprintf "RegPol(%s, %s, %d)" (predicate_to_string pr) (regex_to_string reg) k
  | RegUnion(reg_pol1, reg_pol2) -> Printf.sprintf "(%s <+> %s)" (regexPol_to_string reg_pol1) (regexPol_to_string reg_pol2)
  | RegInter(reg_pol1, reg_pol2) -> Printf.sprintf "(%s <*> %s)" (regexPol_to_string reg_pol1) (regexPol_to_string reg_pol2)

(* Normalization algorithm:
   1) Push intersections till they're over atomic policies or other intersection (DNF)
   2) Eliminate intersections
   3) Find a minimal independent covering set
*)


let rec flatten_reg pol = match pol with
  | Hop sw -> [Hop sw]
  | Host sw -> [Host sw]
  | Star -> [Star]
  (* | Option reg1 reg2 -> [Option reg1 reg2] *)
  | Sequence (reg1, reg2) -> (flatten_reg reg1) @ (flatten_reg reg2)

let rec collapse_star pol = match pol with
  | Star :: Star :: pol -> collapse_star (Star :: pol)
  | a :: pol -> a :: collapse_star pol
  | [] -> []

let get_path topo s1 s2 = let path = G.shortest_path topo s1 s2 in
			  (* let () = Printf.printf "[regex] get_path %Ld %Ld:\n" s1 s2 in *)
			  (* let () =  List.iter (fun x -> Printf.printf "\t%Ld\n" x ) path in *)
			  List.map (fun x -> Hop x) path

let get_path1 topo src dst = List.tl (get_path topo src dst)

let rec expand_path1 path hop topo = match path with
  | Star :: Hop s :: path -> get_path1 topo hop s @ expand_path1 path s topo
  | Hop s1 :: path -> Hop s1 :: expand_path1 path s1 topo
  | _ -> path

let rec star_out_path path last = match path with
  | [a] -> [(a, last)]
  | a :: path -> (a, Star) :: star_out_path path last
  
(* Input: path starting and ending with hosts
   Output: path starting and ending with hosts with no (Star, Host) (Host,Star) transitions 
*)
let rec install_hosts path topo = match path with
  | Host h1 :: Star :: [Host h2] -> 
    let Some (s1,_) = G.get_host_port topo h1 in
    let Some (s2,_) = G.get_host_port topo h2 in
    if s1 = s2 then
      (Host h1, Host h1) :: (Hop s1, Star) :: [(Host h2, Host h2)]
    else
      (Host h1, Host h1) :: (Hop s1, Star) :: (Star, Star) :: (Hop s2, Star) :: [(Host h2, Host h2)]
  | Host h1 :: Star :: path -> 
    let Some (s1,_) = G.get_host_port topo h1 in
    (Host h1, Host h1) :: (Hop s1, Star) ::  install_hosts (Star :: path) topo
  | Star :: [Host h1] -> 
    let Some (s1,_) = G.get_host_port topo h1 in
    (Hop s1, Star) ::  [(Host h1, Host h1)]
  | h :: path -> (h, h) :: install_hosts path topo
  | [] -> []

let rec star_out_regex regex = match regex with
  | [a] -> [(a, a)]
  | a :: regex -> (a, Star) :: star_out_regex regex

let rec expand_path path topo = match fst (List.split (install_hosts path topo)) with
  | Host h1 :: Hop s1 :: path -> Host h1 :: Hop s1 :: expand_path1 path s1 topo

let rec expand_regex_with_match regex hop topo = 
  Printf.printf "[Regex.ml] expand_path_with_match %s\n%!" (String.concat ";" (List.map regex_to_string regex));
  match regex with
  | Star :: Hop s :: path -> star_out_regex (get_path1 topo hop s) @ expand_regex_with_match regex s topo
  | Hop s1 :: path -> (Hop s1, Hop s1) :: expand_regex_with_match path s1 topo
  | [Host h1] -> [(Host h1, Host h1)]

(* Expands a regex into an explicit path starting at hop, storing the "match expansion" along the way *)
let rec expand_path_with_match1 path hop topo = 
  Printf.printf "[Regex.ml] expand_path_with_match1 %Ld %s\n%!" hop (String.concat ";" (List.map (fun (a,b) -> 
    Printf.sprintf "(%s, %s)" (regex_to_string a) (regex_to_string b)) path));
  match path with
  | (Star,_) :: (Hop s, a) :: path -> (star_out_path (get_path1 topo hop s) a) @ expand_path_with_match1 path s topo
  | (Hop s1, a) :: path -> (Hop s1, a) :: expand_path_with_match1 path s1 topo
  | (Star,_) :: [(Host h1, a)] -> 
    let Some (s1,_) = G.get_host_port topo h1 in
    (star_out_path (get_path1 topo hop s1) Star) @  [(Host h1, a)]
  | [(Host h1, a)] -> [(Host h1, a)]

let expand_path_with_match_bad_links regex sw topo bad_links =
  Printf.printf "[Regex.ml] expand_path_with_match_bad_links %Ld %s %s\n%!" sw (String.concat ";" (List.map (fun (a,b) -> 
    Printf.sprintf "(%s, %s)" (regex_to_string a) (regex_to_string b)) regex)) (String.concat ";" (List.map (fun (a,b) -> 
    Printf.sprintf "(%Ld,%Ld)" a b) bad_links));
  let new_topo = G.copy topo in
  G.del_links new_topo bad_links;
  expand_path_with_match1 regex sw new_topo


let rec expand_path_with_match path topo = match install_hosts path topo with
  | (Host h1, a) :: (Hop s1, b) :: path -> (Host h1, a) :: (Hop s1, b) :: expand_path_with_match1 path s1 topo

  (* Naive compilation: does not guarantee loop-free semantics
     Possible issues:
     1) reg contains an explicit loop
     2) We compile star paths to contain the same node

     Possible solutions:
     1) Second compilation phase that detects repeated nodes and tags packets inbetween such repeats
  *)

let bad_hop_handler s1 s2 sw pt pk = ()
  (* Printf.printf "Can not forward pkt from %Ld to %Ld\n" s1 s2 *)
 
let rec compile_path1 pred path topo port = match path with
  | Hop s1 :: Hop s2 :: path -> 
    let p1,p2 = G.get_ports topo s1 s2 in
    Par ((Pol ((And (pred, (And (InPort port,Switch s1)))), [To (unmodified, p1)])), ((compile_path1 pred ((Hop s2) :: path) topo p2)))
  | Hop s1 :: [Host h] -> 
    let Some (_,p1) = G.get_host_port topo h in
    Pol ((And (pred, (And (InPort port,Switch s1)))), 
	 [To ({unmodified with NetCoreEval0x04.modifyDlVlan=(Some None)}, p1)])
  | _ -> Pol (pred, [])

let compile_path pred path topo (vid : WordInterface.Word16.t)  = match path with
  | Host h1 :: Hop s :: [Host h2] -> 
    let Some (s1,p1) = G.get_host_port topo h1 in 
    let Some (s2,p2) = G.get_host_port topo h2 in
    (* assert s1 = s *)
    Pol ((And (pred, (And (InPort p1,Switch s)))), [To (unmodified, p2)])
  | Host h :: Hop s1 :: Hop s2 :: path -> 
    let Some (s1,inport) = G.get_host_port topo h in
    (* assert s1 = s *)
    let p1,p2 = G.get_ports topo s1 s2 in
    let pol = Pol (And (pred, (And (InPort inport,Switch s1))), 
		   [To ({unmodified with NetCoreEval0x04.modifyDlVlan=(Some (Some vid))}, p1)]) in
    Par (pol, compile_path1 (And (DlVlan (Some vid), And (pred, DlVlanPcp 0))) (Hop s2 :: path) topo p2)



module Gensym =
struct
  let count = ref 0
  let next () = incr count; !count
end

let rec compile_regex pol topo = match pol with
  | RegPol (pred, reg, _) -> compile_path pred (expand_path (collapse_star (flatten_reg reg)) topo) topo (Gensym.next ())
  | RegUnion (pol1, pol2) -> Par (compile_regex pol1 topo, compile_regex pol2 topo)

let rec del_links path topo = match path with
  | Host h :: path -> del_links path topo
  | Hop s1 :: Hop s2 :: path -> let p1,p2 = G.get_ports topo s1 s2 in
				G.del_edge topo s1 p1; G.del_edge topo s2 p2; del_links path topo
  | Hop s :: [Host h] -> ()

let get_links _ _ = []
