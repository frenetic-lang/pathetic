open Printf
open Unix
open Pathetic.Regex
open Pathetic.RegexUtils
open NetCoreFT

let test = RegUnion(RegPol(DlType 0, Star, 0),
		    RegUnion(RegPol(DlType 1, Star, 1),
			     RegInter(RegPol(DlType 2, Star, 2),
				      RegPol(DlType 3, Star, 3))))

let main () = 
  Sys.catch_break true;
  Misc.Log.printf "[test] %s\n%!" (regexPol_to_string test);
  Misc.Log.printf "[test] [%s]\n%!" (regexPol_to_string (to_dnf test));
  Misc.Log.printf "[test] [%s]\n%!" (regexPol_to_string (blast_inter (to_dnf test)));
  Misc.Log.printf "[test] [%s]\n%!" (String.concat ";" (List.map regexPol_to_string (normalize test)));
  exit 1
      
let _ = main ()
