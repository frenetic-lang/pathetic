open OpenFlow0x04_Core
open OpenFlow0x04_Platform
open FaultTolerance
open NetCoreFT
open Pathetic.Regex
module H = Hashtbl
module G = Graph.Graph

module D = DiamondTopo
(* module D = IDSTopo *)

module Routing = struct

    (* Diamond topology *)
    (* S1
       /\
     S2 S3
      \ /
       S4
    *)

  let (policy, push) = Lwt_stream.create ()
  let (return_stream, return_push') = Lwt_stream.create ()
  let return_push (swId : int64) (portId : int32) (status : portState) = return_push' (Some (swId,portId,status))

  let ints_to_ipv4 (a,b,c,d) =
    let (|||) x y = Int32.logor x y in
    let (<<<) x y = Int32.shift_left x y in
    let a = Int32.of_int a in
    let b = Int32.of_int b in
    let c = Int32.of_int c in
    let d = Int32.of_int d in
    (a <<< 24) ||| (b <<< 16) ||| (c <<< 8) ||| d

  let make_host_ip i = ints_to_ipv4 (10,0,0,i)
  let h1 = G.Host 1
  let h2 = G.Host 2

  let ids = G.Switch (Int64.of_int 5)

  let from_to i j = And (SrcIP (make_host_ip i), DstIP (make_host_ip j))
			   (* (DlType 0x800)) *)
  let make_policy = RegUnion (RegPol (from_to 1 2, (Sequence (Const h1, Sequence (Star, Const h2))), 1),
                            RegPol (from_to 2 1, (Sequence (Const h2, Sequence (Star, Const h1))), 1))
  (* let make_policy = RegUnion (RegPol (from_to 1 2, (Sequence (Const h1, Sequence (Star, (Sequence (Const ids, Sequence (Star, Const h2)))))), 1), *)
  (*                           RegPol (from_to 2 1, (Sequence (Const h2, Sequence (Star, Const h1))), 1)) *)

  let desugar_group_htbl tbl =
    Hashtbl.fold (fun sw swTbl acc -> 
      let () = Hashtbl.add acc sw (List.map (fun (a,b, acts) -> (a,b, List.map (List.map NetCoreFT.desugar_act) acts)) swTbl) in
    acc) tbl (Hashtbl.create 10)
  (** Composes learning and routing policies, which together form
      mac-learning. *)      
  let groups_to_string groups =
    String.concat ";\n" (List.map (fun (gid,_,acts) -> Printf.sprintf "%ld" gid) groups)

  let group_htbl_to_str ghtbl =
    String.concat "" (H.fold (fun sw groups acc -> (Printf.sprintf "%Ld -> [\n%s]\n" sw (groups_to_string groups)):: acc) ghtbl [])

  let rec port_status_loop () =
    Lwt_stream.map (fun (swId, portId, (status : portState)) ->
      Printf.printf "[RegexFTTest] Got port update from %Ld %ld\n%!" swId portId;
      (swId, portId, status))
      return_stream;
    ()


  let () = let pol = compile_ft_to_nc make_policy (D.make_topo ()) in
	   Printf.printf "%s\n" (policy_to_string pol);
	   push (Some pol);
	   let _ = port_status_loop () in
	   ()

end

module Make (Platform : PLATFORM) = struct

  module Controller = NetCoreFT.Make (Platform)

  let start () = Controller.start_controller Routing.policy Routing.return_push

end

