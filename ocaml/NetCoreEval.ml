open Packet
open OpenFlow0x01Types

(** val filter_map_body :
    ('a1 -> 'a2 option) -> 'a1 -> 'a2 list -> 'a2 list **)

let filter_map_body f a bs =
  match f a with
  | Some b -> b :: bs
  | None -> bs

(** val filter_map : ('a1 -> 'a2 option) -> 'a1 list -> 'a2 list **)

let filter_map f lst =
  List.fold_right (filter_map_body f) lst []

type id =
  int
  (* singleton inductive, whose constructor was MkId *)

type modification = { modifyDlSrc : dlAddr option;
                      modifyDlDst : dlAddr option;
                      modifyDlVlan : dlVlan option;
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

type act = { modifications : modification; toPorts : pseudoPort list;
             queries : id list }

(** val modifications : act -> modification **)

let modifications x = x.modifications

(** val toPorts : act -> pseudoPort list **)

let toPorts x = x.toPorts

(** val queries : act -> id list **)

let queries x = x.queries

(** val empty_action : act **)

let empty_action =
  { modifications = unmodified; toPorts = []; queries = [] }

(** val is_some : 'a1 option -> bool **)

let is_some = function
| Some a -> true
| None -> false

(** val mod_mask : modification -> Pattern.pattern **)

let mod_mask mod0 =
  let { modifyDlSrc = dlSrc0; modifyDlDst = dlDst0; modifyDlVlan = dlVlan0;
    modifyDlVlanPcp = dlVlanPcp0; modifyNwSrc = nwSrc; modifyNwDst = nwDst;
    modifyNwTos = nwTos0; modifyTpSrc = tpSrc; modifyTpDst = tpDst } = mod0
  in
  List.fold_right Pattern.Pattern.inter
    ((if is_some dlSrc0
      then Pattern.Pattern.dlSrc 0L
      else Pattern.Pattern.all) :: ([if is_some dlDst0
                                     then Pattern.Pattern.dlDst 0L
                                     else Pattern.Pattern.all]))
    Pattern.Pattern.all

(** val action_mask : act -> Pattern.pattern **)

let action_mask a =
  mod_mask a.modifications

(** val par_action : act -> act -> act **)

let par_action a1 a2 =
  let { modifications = m1; toPorts = p1; queries = q1 } = a1 in
  let { modifications = m2; toPorts = p2; queries = q2 } = a2 in
  { modifications = m1; toPorts = (List.append p1 p2); queries = (List.append q1 q2) }

(** val override : 'a1 option -> 'a1 option -> 'a1 option **)

let override x y = match y with
| Some a -> y
| None -> x

(** val seq_mod : modification -> modification -> modification **)

let seq_mod m1 m2 =
  let { modifyDlSrc = dlSrc1; modifyDlDst = dlDst1; modifyDlVlan = dlVlan1;
    modifyDlVlanPcp = dlVlanPcp1; modifyNwSrc = nwSrc1; modifyNwDst = nwDst1;
    modifyNwTos = nwTos1; modifyTpSrc = tpSrc1; modifyTpDst = tpDst1 } = m1
  in
  let { modifyDlSrc = dlSrc2; modifyDlDst = dlDst2; modifyDlVlan = dlVlan2;
    modifyDlVlanPcp = dlVlanPcp2; modifyNwSrc = nwSrc2; modifyNwDst = nwDst2;
    modifyNwTos = nwTos2; modifyTpSrc = tpSrc2; modifyTpDst = tpDst2 } = m2
  in
  { modifyDlSrc = (override dlSrc1 dlSrc2); modifyDlDst =
  (override dlDst1 dlDst2); modifyDlVlan = (override dlVlan1 dlVlan2);
  modifyDlVlanPcp = (override dlVlanPcp1 dlVlanPcp2); modifyNwSrc =
  (override nwSrc1 nwSrc2); modifyNwDst = (override nwDst1 nwDst2);
  modifyNwTos = (override nwTos1 nwTos2); modifyTpSrc =
  (override tpSrc1 tpSrc2); modifyTpDst = (override tpDst1 tpDst2) }

(** val seq_action : act -> act -> act **)

let seq_action a1 a2 =
  let { modifications = m1; toPorts = p1; queries = q1 } = a1 in
  let { modifications = m2; toPorts = p2; queries = q2 } = a2 in
  { modifications = (seq_mod m1 m2); toPorts = p2; queries = (List.append q1 q2) }

type pred =
| PrHdr of Pattern.pattern
| PrOnSwitch of switchId
| PrOr of pred * pred
| PrAnd of pred * pred
| PrNot of pred
| PrAll
| PrNone

type pol =
| PoAtom of pred * act
| PoUnion of pol * pol
| PoSeq of pol * pol

type input =
| InPkt of switchId * portId * packet * bufferId option

type output =
| OutPkt of switchId * pseudoPort * packet * (bufferId, bytes) Datatypes.sum
| OutGetPkt of id * switchId * portId * packet
| OutNothing

(** val is_OutPkt : output -> bool **)

let is_OutPkt = function
| OutPkt (s, p, p0, s0) -> true
| _ -> false

(** val match_pred : pred -> switchId -> portId -> packet -> bool **)

let rec match_pred pr sw pt pk =
  match pr with
  | PrHdr pat -> Pattern.Pattern.match_packet pt pk pat
  | PrOnSwitch sw' -> if sw = sw' then true else false
  | PrOr (p1, p2) -> (||) (match_pred p1 sw pt pk) (match_pred p2 sw pt pk)
  | PrAnd (p1, p2) -> (&&) (match_pred p1 sw pt pk) (match_pred p2 sw pt pk)
  | PrNot p' -> not (match_pred p' sw pt pk)
  | PrAll -> true
  | PrNone -> false

(** val serialize_pkt : packet -> bytes **)

let serialize_pkt = Packet.marshal

(** val maybe_modify :
    'a1 option -> (packet -> 'a1 -> packet) -> packet -> packet **)

let maybe_modify newVal modifier pk =
  match newVal with
  | Some v -> modifier pk v
  | None -> pk

(** val withVlanNone : dlVlan option option -> dlVlan option **)

let withVlanNone = function
| Some y ->
  (match y with
   | Some n -> Some n
   | None -> Some coq_VLAN_NONE)
| None -> None

(** val modify_pkt : modification -> packet -> packet **)

let modify_pkt mods pk =
  let { modifyDlSrc = dlSrc0; modifyDlDst = dlDst0; modifyDlVlan = dlVlan0;
    modifyDlVlanPcp = dlVlanPcp0; modifyNwSrc = nwSrc; modifyNwDst = nwDst;
    modifyNwTos = nwTos0; modifyTpSrc = tpSrc; modifyTpDst = tpDst } = mods
  in
  maybe_modify dlSrc0 setDlSrc
    (maybe_modify dlDst0 setDlDst
      (maybe_modify dlVlan0 setDlVlan
        (maybe_modify dlVlanPcp0 setDlVlanPcp
          (maybe_modify nwSrc setNwSrc
            (maybe_modify nwDst setNwDst
              (maybe_modify nwTos0 setNwTos
                (maybe_modify tpSrc setTpSrc
                  (maybe_modify tpDst setTpDst pk))))))))

(** val outp_to_inp : output -> input option **)

let outp_to_inp = function
| OutPkt (sw, p, pk, s) ->
  (match p with
   | PhysicalPort pt ->
     (match s with
      | Datatypes.Coq_inl bufId -> Some (InPkt (sw, pt, pk, (Some bufId)))
      | Datatypes.Coq_inr b -> None)
   | _ -> None)
| _ -> None

(** val eval_action : input -> act -> output list **)

let eval_action inp act0 =
  let { modifications = mods; toPorts = ports; queries = queries0 } = act0 in
  let InPkt (sw, pt, pk, buf) = inp in
  List.append
    (List.map (fun pt0 -> OutPkt (sw, pt0, (modify_pkt mods pk),
      (match buf with
       | Some b -> Datatypes.Coq_inl b
       | None -> Datatypes.Coq_inr (serialize_pkt (modify_pkt mods pk))))) ports)
    (List.map (fun qid -> OutGetPkt (qid, sw, pt, pk)) queries0)

(** val classify : pol -> input -> output list **)

let rec classify p inp =
  match p with
  | PoAtom (pr, actions) ->
    let InPkt (sw, pt, pk, buf) = inp in
    eval_action inp
      (if match_pred pr sw pt pk then actions else empty_action)
  | PoUnion (p1, p2) -> List.append (classify p1 inp) (classify p2 inp)
  | PoSeq (p1, p2) ->
    let (outPkts1, queries1) = List.partition is_OutPkt (classify p1 inp) in
    List.append queries1 (List.concat (List.map (classify p2) (filter_map outp_to_inp outPkts1)))

