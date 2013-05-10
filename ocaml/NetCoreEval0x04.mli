open Datatypes
open WordInterface
open NetworkPacket
open OpenFlowTypes

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

val modifyDlSrc : modification -> dlAddr option
val modifyDlDst : modification -> dlAddr option
val modifyDlVlan : modification -> dlVlan option option
val modifyDlVlanPcp : modification -> dlVlanPcp option
val modifyNwSrc : modification -> nwAddr option
val modifyNwDst : modification -> nwAddr option
val modifyNwTos : modification -> nwTos option
val modifyTpSrc : modification -> tpPort option
val modifyTpDst : modification -> tpPort option
val unmodified : modification

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
| PoOpt of pred * act list

type input =
| InPkt of switchId * portId * packet * bufferId option

type output =
| OutAct of switchId * act list * packet * (bufferId, bytes) sum
| OutGetPkt of id * switchId * portId * packet
| OutNothing

val classify : pol -> input -> output list
