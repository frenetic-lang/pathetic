open Printf
(* open OpenFlow0x04Parser *)
open OpenFlow0x01_PlatformSig
open Unix
(* open OpenFlow0x04_Core *)
module Test = RegexTest
(* module Test = RegexFTTest *)

(* module Controller = RegexTest.Make (OpenFlowPlatform) *)
module Controller = Test.Make (OpenFlow0x01_Platform)

(* configuration state *)
let controller = ref "learn"

(* command-line arguments *)
let arg_specs = 
  [ ("-c", 
     Arg.Set_string controller, 
     "<controller> run a specific controller")
  ]
 
let arg_rest rest = ()

let usage = 
  "desmoines [options]"

let () = Arg.parse arg_specs arg_rest usage

let main () = 
  Sys.catch_break true;
  try 
    OpenFlow0x01_Platform.init_with_port 6633;
    (* Printexc.record_backtrace (); *)
    Lwt_main.run (Controller.start ())
  with exn -> 
    OpenFlow0x04_Misc.Log.printf "[main] exception: %s\n%s\n%!" 
      (Printexc.to_string exn) (Printexc.get_backtrace ());
    OpenFlow0x01_Platform.shutdown ();
    exit 1
      
let _ = main ()
