open NetworkPacket
open WordInterface

(** val coq_VLAN_NONE : dlVlan **)

let coq_VLAN_NONE = 65535

type 'a mask = { m_value : 'a; m_mask : 'a option }

(** val m_value : 'a1 mask -> 'a1 **)

let m_value x = x.m_value

(** val m_mask : 'a1 mask -> 'a1 option **)

let m_mask x = x.m_mask

type xid = Word32.t

(** val val_to_mask : 'a1 -> 'a1 mask **)

let val_to_mask v =
  { m_value = v; m_mask = None }

type switchId = Word64.t

type groupId = Word32.t

type portId = Word32.t

type tableId = Word8.t

type bufferId = Word32.t

type oxm =
| OxmInPort of portId
| OxmInPhyPort of portId
| OxmMetadata of Word64.t mask
| OxmEthType of Word16.t
| OxmEthDst of Word48.t mask
| OxmEthSrc of Word48.t mask
| OxmVlanVId of Word12.t mask
| OxmVlanPcp of Word8.t
| OxmIPProto of Word8.t
| OxmIPDscp of Word8.t
| OxmIPEcn of Word8.t
| OxmIP4Src of Word32.t mask
| OxmIP4Dst of Word32.t mask
| OxmTCPSrc of Word16.t mask
| OxmTCPDst of Word16.t mask
| OxmARPOp of Word16.t
| OxmARPSpa of Word32.t mask
| OxmARPTpa of Word32.t mask
| OxmARPSha of Word48.t mask
| OxmARPTha of Word48.t mask
| OxmICMPType of Word8.t
| OxmICMPCode of Word8.t
| OxmMPLSLabel of Word32.t
| OxmMPLSTc of Word8.t
| OxmTunnelId of Word64.t mask

type oxmMatch = oxm list

type pseudoPort =
| PhysicalPort of portId
| InPort
| Flood
| AllPorts
| Controller of Word16.t
| Any

type action =
| Output of pseudoPort
| Group of groupId
| PopVlan
| PushVlan
| PopMpls
| PushMpls
| SetField of oxm

type actionSequence = action list

type instruction =
| GotoTable of tableId
| ApplyActions of actionSequence
| WriteActions of actionSequence

type bucket = { bu_weight : Word16.t; bu_watch_port : portId option;
                bu_watch_group : groupId option; bu_actions : actionSequence }

(** val bu_weight : bucket -> Word16.t **)

let bu_weight x = x.bu_weight

(** val bu_watch_port : bucket -> portId option **)

let bu_watch_port x = x.bu_watch_port

(** val bu_watch_group : bucket -> groupId option **)

let bu_watch_group x = x.bu_watch_group

(** val bu_actions : bucket -> actionSequence **)

let bu_actions x = x.bu_actions

type groupType =
| All
| Select
| Indirect
| FF

type groupMod =
| AddGroup of groupType * groupId * bucket list
| DeleteGroup of groupType * groupId

type timeout =
| Permanent
| ExpiresAfter of Word16.t

type flowModCommand =
| AddFlow
| ModFlow
| ModStrictFlow
| DeleteFlow
| DeleteStrictFlow

type flowModFlags = { fmf_send_flow_rem : bool; fmf_check_overlap : bool;
                      fmf_reset_counts : bool; fmf_no_pkt_counts : bool;
                      fmf_no_byt_counts : bool }

(** val fmf_send_flow_rem : flowModFlags -> bool **)

let fmf_send_flow_rem x = x.fmf_send_flow_rem

(** val fmf_check_overlap : flowModFlags -> bool **)

let fmf_check_overlap x = x.fmf_check_overlap

(** val fmf_reset_counts : flowModFlags -> bool **)

let fmf_reset_counts x = x.fmf_reset_counts

(** val fmf_no_pkt_counts : flowModFlags -> bool **)

let fmf_no_pkt_counts x = x.fmf_no_pkt_counts

(** val fmf_no_byt_counts : flowModFlags -> bool **)

let fmf_no_byt_counts x = x.fmf_no_byt_counts

type flowMod = { mfCookie : Word64.t mask; mfTable_id : tableId;
                 mfCommand : flowModCommand; mfIdle_timeout : timeout;
                 mfHard_timeout : timeout; mfPriority : Word16.t;
                 mfBuffer_id : bufferId option;
                 mfOut_port : pseudoPort option;
                 mfOut_group : groupId option; mfFlags : flowModFlags;
                 mfOfp_match : oxmMatch; mfInstructions : instruction list }

(** val mfCookie : flowMod -> Word64.t mask **)

let mfCookie x = x.mfCookie

(** val mfTable_id : flowMod -> tableId **)

let mfTable_id x = x.mfTable_id

(** val mfCommand : flowMod -> flowModCommand **)

let mfCommand x = x.mfCommand

(** val mfIdle_timeout : flowMod -> timeout **)

let mfIdle_timeout x = x.mfIdle_timeout

(** val mfHard_timeout : flowMod -> timeout **)

let mfHard_timeout x = x.mfHard_timeout

(** val mfPriority : flowMod -> Word16.t **)

let mfPriority x = x.mfPriority

(** val mfBuffer_id : flowMod -> bufferId option **)

let mfBuffer_id x = x.mfBuffer_id

(** val mfOut_port : flowMod -> pseudoPort option **)

let mfOut_port x = x.mfOut_port

(** val mfOut_group : flowMod -> groupId option **)

let mfOut_group x = x.mfOut_group

(** val mfFlags : flowMod -> flowModFlags **)

let mfFlags x = x.mfFlags

(** val mfOfp_match : flowMod -> oxmMatch **)

let mfOfp_match x = x.mfOfp_match

(** val mfInstructions : flowMod -> instruction list **)

let mfInstructions x = x.mfInstructions

type packetInReason =
| NoMatch
| ExplicitSend

type packetIn = { pi_buffer_id : Word32.t option; pi_total_len : Word16.t;
                  pi_reason : packetInReason; pi_table_id : tableId;
                  pi_cookie : Word64.t; pi_ofp_match : oxmMatch;
                  pi_pkt : packet option }

(** val pi_buffer_id : packetIn -> Word32.t option **)

let pi_buffer_id x = x.pi_buffer_id

(** val pi_total_len : packetIn -> Word16.t **)

let pi_total_len x = x.pi_total_len

(** val pi_reason : packetIn -> packetInReason **)

let pi_reason x = x.pi_reason

(** val pi_table_id : packetIn -> tableId **)

let pi_table_id x = x.pi_table_id

(** val pi_cookie : packetIn -> Word64.t **)

let pi_cookie x = x.pi_cookie

(** val pi_ofp_match : packetIn -> oxmMatch **)

let pi_ofp_match x = x.pi_ofp_match

(** val pi_pkt : packetIn -> packet option **)

let pi_pkt x = x.pi_pkt

type capabilities = { flow_stats : bool; table_stats : bool;
                      port_stats : bool; group_stats : bool; ip_reasm : 
                      bool; queue_stats : bool; port_blocked : bool }

type portState = { link_down : bool; blocked : bool; live : bool }

type portDesc = { port_no : portId;
		  (* hw_addr : Word48.t; *)
		  (* name; *)
		  (* config; *)
		  state : portState
		  (* curr; *)
		  (* advertised; *)
		  (* supported; *)
		  (* peer; *)
		  (* curr_speed; *)
		  (* max_speed *) }

type portReason =
  | PortAdd 
  | PortDelete
  | PortModify

type portStatus = { reason : portReason; desc : portDesc }

(** val flow_stats : capabilities -> bool **)

let flow_stats x = x.flow_stats

(** val table_stats : capabilities -> bool **)

let table_stats x = x.table_stats

(** val port_stats : capabilities -> bool **)

let port_stats x = x.port_stats

(** val group_stats : capabilities -> bool **)

let group_stats x = x.group_stats

(** val ip_reasm : capabilities -> bool **)

let ip_reasm x = x.ip_reasm

(** val queue_stats : capabilities -> bool **)

let queue_stats x = x.queue_stats

(** val port_blocked : capabilities -> bool **)

let port_blocked x = x.port_blocked

type features = { datapath_id : Word64.t; num_buffers : Word32.t;
                  num_tables : Word8.t; aux_id : Word8.t;
                  supported_capabilities : capabilities }

(** val datapath_id : features -> Word64.t **)

let datapath_id x = x.datapath_id

(** val num_buffers : features -> Word32.t **)

let num_buffers x = x.num_buffers

(** val num_tables : features -> Word8.t **)

let num_tables x = x.num_tables

(** val aux_id : features -> Word8.t **)

let aux_id x = x.aux_id

(** val supported_capabilities : features -> capabilities **)

let supported_capabilities x = x.supported_capabilities

type packetOut = { po_buffer_id : bufferId option; po_in_port : pseudoPort;
                   po_actions : actionSequence; po_pkt : packet option }

(** val po_buffer_id : packetOut -> bufferId option **)

let po_buffer_id x = x.po_buffer_id

(** val po_in_port : packetOut -> pseudoPort **)

let po_in_port x = x.po_in_port

(** val po_actions : packetOut -> actionSequence **)

let po_actions x = x.po_actions

(** val po_pkt : packetOut -> packet option **)

let po_pkt x = x.po_pkt

type message =
| Hello
| EchoRequest of bytes
| EchoReply of bytes
| FeaturesRequest
| FeaturesReply of features
| FlowModMsg of flowMod
| GroupModMsg of groupMod
| PacketInMsg of packetIn
| PacketOutMsg of packetOut
| PortStatusMsg of portStatus
| BarrierRequest
| BarrierReply
