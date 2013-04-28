
open List
open Datatypes
open Classifier
open Types
open NetCoreEval0x04

type __ = Obj.t
let __ = let rec f _ = Obj.repr f in Obj.repr f

let rec compile_pred opt pr sw =
  match pr with
  | PrHdr pat -> (pat, true) :: []
  | PrOnSwitch sw' ->
    if sw == sw' then (Pattern.Pattern.all, true) :: [] else []
  | PrOr (pr1, pr2) ->
    opt (union (||) (compile_pred opt pr1 sw) (compile_pred opt pr2 sw))
  | PrAnd (pr1, pr2) ->
    opt (inter (&&) (compile_pred opt pr1 sw) (compile_pred opt pr2 sw))
  | PrNot pr' ->
    opt
      (map (second negb)
        (app (compile_pred opt pr' sw) ((Pattern.Pattern.all, false) :: [])))
  | PrAll -> (Pattern.Pattern.all, true) :: []
  | PrNone -> []

let apply_act a = function
| true -> a
| false -> []

let rec compile_pol opt p sw =
  match p with
  | PoAtom (pr, act0) ->
    opt
      (map (second (apply_act act0))
        (app (compile_pred (Obj.magic opt __) pr sw)
          ((Pattern.Pattern.all,false)::[])))
  | PoUnion (pol1, pol2) ->
    opt
      (union app (compile_pol opt pol1 sw) (compile_pol opt pol2 sw))

(** val strip_empty_rules : 'a1 coq_Classifier -> 'a1 coq_Classifier **)

let rec strip_empty_rules = function
| [] -> []
| p::cf0 ->
  let pat,acts = p in
  if Pattern.Pattern.is_empty pat
  then strip_empty_rules cf0
  else (pat,acts)::(strip_empty_rules cf0)

(** val no_opt : 'a1 coq_Classifier -> 'a1 coq_Classifier **)

let no_opt x = id x

(** val compile_no_opt : pol -> switchId -> act list coq_Classifier **)

let compile_no_opt =
  compile_pol no_opt

(** val compile_opt : pol -> switchId -> act list coq_Classifier **)

let compile_opt pol swid =
  (compile_pol (fun x -> strip_empty_rules (elim_shadowed x)) pol swid, [])

