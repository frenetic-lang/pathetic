open WordInterface

open Platform0x04
open NetCoreFT

module G = Graph.Graph

module Routing = struct

  let s101 = G.Switch (Int64.of_int 1)
  let s102 = G.Switch (Int64.of_int 2)
  let s103 = G.Switch (Int64.of_int 3)
  let s104 = G.Switch (Int64.of_int 4)
  let h1 = G.Host 1
  let h2 = G.Host 2
  open Pathetic.Regex
  open Pathetic.RegexUtils

  let (policy, push) = Lwt_stream.create ()
    
  let test_regex = RegPol (All, (Const h1 <.> Star <.> Const h2), 0) 
    <+>
      RegPol (All, (Const h2 <.> Star <.> Const h1), 0)

  let () = let pol = (compile_regex test_regex (DiamondTopo.make_topo ())) in
	   Printf.printf "%s\n" (policy_to_string pol);
	   push (Some pol)
end

module Make (Platform : PLATFORM) = struct

  module Controller = NetCoreFT.Make (Platform)

  let start () = Controller.start_controller Routing.policy

end

