open Datatypes
open NetworkPacket
open OpenFlowTypes
open WordInterface

type id = int

type modification = { modifyDlSrc : dlAddr option;
                      modifyDlDst : dlAddr option;
                      modifyDlVlan : dlVlan option option;
                      modifyDlVlanPcp : dlVlanPcp option;
                      modifyNwSrc : nwAddr option;
                      modifyNwDst : nwAddr option;
                      modifyNwTos : nwTos option;
                      modifyTpSrc : tpPort option;
                      modifyTpDst : tpPort option }

(** val modifyDlSrc : modification -> dlAddr option **)

let modifyDlSrc x = x.modifyDlSrc

(** val modifyDlDst : modification -> dlAddr option **)

let modifyDlDst x = x.modifyDlDst

(** val modifyDlVlan : modification -> dlVlan option option **)

let modifyDlVlan x = x.modifyDlVlan

(** val modifyDlVlanPcp : modification -> dlVlanPcp option **)

let modifyDlVlanPcp x = x.modifyDlVlanPcp

(** val modifyNwSrc : modification -> nwAddr option **)

let modifyNwSrc x = x.modifyNwSrc

(** val modifyNwDst : modification -> nwAddr option **)

let modifyNwDst x = x.modifyNwDst

(** val modifyNwTos : modification -> nwTos option **)

let modifyNwTos x = x.modifyNwTos

(** val modifyTpSrc : modification -> tpPort option **)

let modifyTpSrc x = x.modifyTpSrc

(** val modifyTpDst : modification -> tpPort option **)

let modifyTpDst x = x.modifyTpDst

(** val unmodified : modification **)

let unmodified =
  { modifyDlSrc = None; modifyDlDst = None; modifyDlVlan = None;
    modifyDlVlanPcp = None; modifyNwSrc = None; modifyNwDst = None;
    modifyNwTos = None; modifyTpSrc = None; modifyTpDst = None }

type pred =
| PrHdr of Pattern.pattern
| PrOnSwitch of switchId
| PrOr of pred * pred
| PrAnd of pred * pred
| PrNot of pred
| PrAll
| PrNone

type act =
| Forward of modification * pseudoPort
| Group of groupId
| ActGetPkt of id

type pol =
| PoAtom of pred * act list
| PoUnion of pol * pol

type input =
| InPkt of switchId * portId * packet * bufferId option

type output =
| OutAct of switchId * act list * packet * (bufferId, bytes) sum
| OutGetPkt of id * switchId * portId * packet
| OutNothing

let maybe_modify newVal modifier pk =
  match newVal with
  | Some v -> modifier pk v
  | None -> pk

(** val withVlanNone : dlVlan option option -> dlVlan option **)

let withVlanNone = function
| Some y ->
  (match y with
   | Some n -> Some n
   | None -> Some OpenFlow0x01Types.coq_VLAN_NONE)
| None -> None

(** val modify_pkt : modification -> packet -> packet **)

let modify_pkt mods pk =
  let { modifyDlSrc = dlSrc; modifyDlDst = dlDst; modifyDlVlan = dlVlan0;
    modifyDlVlanPcp = dlVlanPcp0; modifyNwSrc = nwSrc; modifyNwDst = nwDst;
    modifyNwTos = nwTos0; modifyTpSrc = tpSrc; modifyTpDst = tpDst } = mods
  in
  maybe_modify dlSrc setDlSrc
    (maybe_modify dlDst setDlDst
      (maybe_modify (withVlanNone dlVlan0) setDlVlan
        (maybe_modify dlVlanPcp0 setDlVlanPcp
          (maybe_modify nwSrc setNwSrc
            (maybe_modify nwDst setNwDst
              (maybe_modify nwTos0 setNwTos
                (maybe_modify tpSrc setTpSrc
                  (maybe_modify tpDst setTpDst pk))))))))


(** val classify : pol -> input -> output list **)

(* Need to strip out the controller actions so that we don't cycle packets between the switch and controller *)

let rec strip_controller acts = match acts with
  | Forward (a, (Controller x)) :: acts -> strip_controller acts
  | ActGetPkt x :: acts -> strip_controller acts
  | a :: acts -> a :: strip_controller acts
  | [] -> []

(** val match_pred : pred -> switchId -> portId -> packet -> bool **)

let rec match_pred pr sw pt pk =
  match pr with
  | PrHdr pat -> Pattern.Pattern.match_packet pt pk pat
  | PrOnSwitch sw' -> if Word64.eq_dec sw sw' then true else false
  | PrOr (p1, p2) -> (||) (match_pred p1 sw pt pk) (match_pred p2 sw pt pk)
  | PrAnd (p1, p2) -> (&&) (match_pred p1 sw pt pk) (match_pred p2 sw pt pk)
  | PrNot p' -> negb (match_pred p' sw pt pk)
  | PrAll -> true
  | PrNone -> false


let eval_action inp = function
| Forward (mods, pp) ->
  let InPkt (sw, p, pk, buf) = inp in
  OutAct (sw, [Forward (mods, pp)], pk,
	  (match buf with
	    | Some b -> Coq_inl b
	    | None -> Coq_inr (Packet_Parser.serialize_packet pk)))
| ActGetPkt x ->
  let InPkt (sw, pt, pk, buf) = inp in OutGetPkt (x, sw, pt, pk)

(* [OutAct (sw, (strip_controller actions), pk, match buf with *)
(*       | Some b -> Coq_inl b *)
(*       | None -> Coq_inr (Cstruct.to_string (serialize_pkt pk)))] else [] *)

let rec classify p inp =
  match p with
  | PoAtom (pr, actions) ->
    let InPkt (sw, pt, pk, buf) = inp in
    if match_pred pr sw (Int32.to_int pt) pk then List.map (eval_action inp) actions else []
  | PoUnion (p1, p2) -> app (classify p1 inp) (classify p2 inp)
