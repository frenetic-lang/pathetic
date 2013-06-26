open Datatypes
open Packet

type __ = Obj.t

type portId = int16

val coq_VLAN_NONE : dlVlan

type of_match = { matchDlSrc : dlAddr option; matchDlDst : dlAddr option;
                  matchDlTyp : dlTyp option; matchDlVlan : dlVlan option;
                  matchDlVlanPcp : dlVlanPcp option;
                  matchNwSrc : nwAddr option; matchNwDst : nwAddr option;
                  matchNwProto : nwProto option; matchNwTos : nwTos option;
                  matchTpSrc : tpPort option; matchTpDst : tpPort option;
                  matchInPort : portId option }

val of_match_rect :
  (dlAddr option -> dlAddr option -> dlTyp option -> dlVlan option ->
  dlVlanPcp option -> nwAddr option -> nwAddr option -> nwProto option ->
  nwTos option -> tpPort option -> tpPort option -> portId option -> 'a1) ->
  of_match -> 'a1

val of_match_rec :
  (dlAddr option -> dlAddr option -> dlTyp option -> dlVlan option ->
  dlVlanPcp option -> nwAddr option -> nwAddr option -> nwProto option ->
  nwTos option -> tpPort option -> tpPort option -> portId option -> 'a1) ->
  of_match -> 'a1

val matchDlSrc : of_match -> dlAddr option

val matchDlDst : of_match -> dlAddr option

val matchDlTyp : of_match -> dlTyp option

val matchDlVlan : of_match -> dlVlan option

val matchDlVlanPcp : of_match -> dlVlanPcp option

val matchNwSrc : of_match -> nwAddr option

val matchNwDst : of_match -> nwAddr option

val matchNwProto : of_match -> nwProto option

val matchNwTos : of_match -> nwTos option

val matchTpSrc : of_match -> tpPort option

val matchTpDst : of_match -> tpPort option

val matchInPort : of_match -> portId option

type capabilities = { flow_stats : bool; table_stats : bool;
                      port_stats : bool; stp : bool; ip_reasm : bool;
                      queue_stats : bool; arp_match_ip : bool }

val capabilities_rect :
  (bool -> bool -> bool -> bool -> bool -> bool -> bool -> 'a1) ->
  capabilities -> 'a1

val capabilities_rec :
  (bool -> bool -> bool -> bool -> bool -> bool -> bool -> 'a1) ->
  capabilities -> 'a1

val flow_stats : capabilities -> bool

val table_stats : capabilities -> bool

val port_stats : capabilities -> bool

val stp : capabilities -> bool

val ip_reasm : capabilities -> bool

val queue_stats : capabilities -> bool

val arp_match_ip : capabilities -> bool

type actions = { output : bool; set_vlan_id : bool; set_vlan_pcp : bool;
                 strip_vlan : bool; set_dl_src : bool; set_dl_dst : bool;
                 set_nw_src : bool; set_nw_dst : bool; set_nw_tos : bool;
                 set_tp_src : bool; set_tp_dst : bool; enqueue : bool;
                 vendor : bool }

val actions_rect :
  (bool -> bool -> bool -> bool -> bool -> bool -> bool -> bool -> bool ->
  bool -> bool -> bool -> bool -> 'a1) -> actions -> 'a1

val actions_rec :
  (bool -> bool -> bool -> bool -> bool -> bool -> bool -> bool -> bool ->
  bool -> bool -> bool -> bool -> 'a1) -> actions -> 'a1

val output : actions -> bool

val set_vlan_id : actions -> bool

val set_vlan_pcp : actions -> bool

val strip_vlan : actions -> bool

val set_dl_src : actions -> bool

val set_dl_dst : actions -> bool

val set_nw_src : actions -> bool

val set_nw_dst : actions -> bool

val set_nw_tos : actions -> bool

val set_tp_src : actions -> bool

val set_tp_dst : actions -> bool

val enqueue : actions -> bool

val vendor : actions -> bool

type features = { switch_id : int64; num_buffers : int32;
                  num_tables : int8;
                  supported_capabilities : capabilities;
                  supported_actions : actions }

val features_rect :
  (int64 -> int32 -> int8 -> capabilities -> actions -> 'a1) ->
  features -> 'a1

val features_rec :
  (int64 -> int32 -> int8 -> capabilities -> actions -> 'a1) ->
  features -> 'a1

val switch_id : features -> int64

val num_buffers : features -> int32

val num_tables : features -> int8

val supported_capabilities : features -> capabilities

val supported_actions : features -> actions

type flowModCommand =
| AddFlow
| ModFlow
| ModStrictFlow
| DeleteFlow
| DeleteStrictFlow

val flowModCommand_rect :
  'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> flowModCommand -> 'a1

val flowModCommand_rec :
  'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> flowModCommand -> 'a1

type switchId = int64

type priority = int16

type bufferId = int32

type pseudoPort =
| PhysicalPort of portId
| InPort
| Flood
| AllPorts
| Controller of int16

val pseudoPort_rect :
  (portId -> 'a1) -> 'a1 -> 'a1 -> 'a1 -> (int16 -> 'a1) -> pseudoPort ->
  'a1

val pseudoPort_rec :
  (portId -> 'a1) -> 'a1 -> 'a1 -> 'a1 -> (int16 -> 'a1) -> pseudoPort ->
  'a1

type action =
| Output of pseudoPort
| SetDlVlan of dlVlan
| SetDlVlanPcp of dlVlanPcp
| StripVlan
| SetDlSrc of dlAddr
| SetDlDst of dlAddr
| SetNwSrc of nwAddr
| SetNwDst of nwAddr
| SetNwTos of nwTos
| SetTpSrc of tpPort
| SetTpDst of tpPort

val action_rect :
  (pseudoPort -> 'a1) -> (dlVlan -> 'a1) -> (dlVlanPcp -> 'a1) -> 'a1 ->
  (dlAddr -> 'a1) -> (dlAddr -> 'a1) -> (nwAddr -> 'a1) -> (nwAddr -> 'a1) ->
  (nwTos -> 'a1) -> (tpPort -> 'a1) -> (tpPort -> 'a1) -> action -> 'a1

val action_rec :
  (pseudoPort -> 'a1) -> (dlVlan -> 'a1) -> (dlVlanPcp -> 'a1) -> 'a1 ->
  (dlAddr -> 'a1) -> (dlAddr -> 'a1) -> (nwAddr -> 'a1) -> (nwAddr -> 'a1) ->
  (nwTos -> 'a1) -> (tpPort -> 'a1) -> (tpPort -> 'a1) -> action -> 'a1

type actionSequence = action list

type timeout =
| Permanent
| ExpiresAfter of int16

val timeout_rect : 'a1 -> (int16 -> __ -> 'a1) -> timeout -> 'a1

val timeout_rec : 'a1 -> (int16 -> __ -> 'a1) -> timeout -> 'a1

type flowMod = { mfModCmd : flowModCommand; mfMatch : of_match;
                 mfPriority : priority; mfActions : actionSequence;
                 mfCookie : int64; mfIdleTimeOut : timeout;
                 mfHardTimeOut : timeout; mfNotifyWhenRemoved : bool;
                 mfApplyToPacket : bufferId option;
                 mfOutPort : pseudoPort option; mfCheckOverlap : bool }

val flowMod_rect :
  (flowModCommand -> of_match -> priority -> actionSequence -> int64 ->
  timeout -> timeout -> bool -> bufferId option -> pseudoPort option -> bool
  -> 'a1) -> flowMod -> 'a1

val flowMod_rec :
  (flowModCommand -> of_match -> priority -> actionSequence -> int64 ->
  timeout -> timeout -> bool -> bufferId option -> pseudoPort option -> bool
  -> 'a1) -> flowMod -> 'a1

val mfModCmd : flowMod -> flowModCommand

val mfMatch : flowMod -> of_match

val mfPriority : flowMod -> priority

val mfActions : flowMod -> actionSequence

val mfCookie : flowMod -> int64

val mfIdleTimeOut : flowMod -> timeout

val mfHardTimeOut : flowMod -> timeout

val mfNotifyWhenRemoved : flowMod -> bool

val mfApplyToPacket : flowMod -> bufferId option

val mfOutPort : flowMod -> pseudoPort option

val mfCheckOverlap : flowMod -> bool

type packetInReason =
| NoMatch
| ExplicitSend

val packetInReason_rect : 'a1 -> 'a1 -> packetInReason -> 'a1

val packetInReason_rec : 'a1 -> 'a1 -> packetInReason -> 'a1

type packetIn = { packetInBufferId : bufferId option;
                  packetInTotalLen : int16; packetInPort : portId;
                  packetInReason_ : packetInReason; packetInPacket : 
                  packet }

val packetIn_rect :
  (bufferId option -> int16 -> portId -> packetInReason -> packet -> 'a1)
  -> packetIn -> 'a1

val packetIn_rec :
  (bufferId option -> int16 -> portId -> packetInReason -> packet -> 'a1)
  -> packetIn -> 'a1

val packetInBufferId : packetIn -> bufferId option

val packetInTotalLen : packetIn -> int16

val packetInPort : packetIn -> portId

val packetInReason_ : packetIn -> packetInReason

val packetInPacket : packetIn -> packet

type xid = int32

type packetOut = { pktOutBufOrBytes : (bufferId, bytes) sum;
                   pktOutPortId : portId option;
                   pktOutActions : actionSequence }

val packetOut_rect :
  ((bufferId, bytes) sum -> portId option -> actionSequence -> 'a1) ->
  packetOut -> 'a1

val packetOut_rec :
  ((bufferId, bytes) sum -> portId option -> actionSequence -> 'a1) ->
  packetOut -> 'a1

val pktOutBufOrBytes : packetOut -> (bufferId, bytes) sum

val pktOutPortId : packetOut -> portId option

val pktOutActions : packetOut -> actionSequence

type message =
| Hello of bytes
| EchoRequest of bytes
| EchoReply of bytes
| FeaturesRequest
| FeaturesReply of features
| FlowModMsg of flowMod
| PacketInMsg of packetIn
| PacketOutMsg of packetOut
| BarrierRequest
| BarrierReply

val message_rect :
  (bytes -> 'a1) -> (bytes -> 'a1) -> (bytes -> 'a1) -> 'a1 -> (features ->
  'a1) -> (flowMod -> 'a1) -> (packetIn -> 'a1) -> (packetOut -> 'a1) -> 'a1
  -> 'a1 -> message -> 'a1

val message_rec :
  (bytes -> 'a1) -> (bytes -> 'a1) -> (bytes -> 'a1) -> 'a1 -> (features ->
  'a1) -> (flowMod -> 'a1) -> (packetIn -> 'a1) -> (packetOut -> 'a1) -> 'a1
  -> 'a1 -> message -> 'a1

