open OpenFlow0x01_PlatformSig
open NetCore_Types
open NetCore_Pretty

module G = Graph.Graph

module Routing = struct

  let s101 = G.Switch (Int64.of_int 1)
  let s102 = G.Switch (Int64.of_int 2)
  let s103 = G.Switch (Int64.of_int 3)
  let s104 = G.Switch (Int64.of_int 4)
  let h1 = G.Host 1
  let h2 = G.Host 2
  open Regex
  open RegexUtils

  let (policy, push) = Lwt_stream.create ()
  let (pkt_stream, pkt_push) = Lwt_stream.create ()
    
  let test_regex = RegPol (Everything, (Const h1 <.> Star <.> Const h2)) 
    <+>
      RegPol (Everything, (Const h2 <.> Star <.> Const h1))

  let () = let pol = (compile_regex test_regex (DiamondTopo.make_topo ())) in
	   Printf.printf "%s\n" (string_of_pol pol);
	   push (Some pol)
end

module Make (Platform : PLATFORM) = struct

  module Controller = NetCore_Controller.Make (Platform)

  let start () = let (_, pol_stream) = NetCore_Stream.from_stream (Action []) Routing.policy in
		 Controller.start_controller Routing.pkt_stream pol_stream

end

