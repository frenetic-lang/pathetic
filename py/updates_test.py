from pox.core import core
from pox.lib.revent import *
import pox.openflow.libopenflow_01 as of
from mininet.log import lg
import networkx as nx
from CustomTopo import topos
import logging
import testFlowGen
from updateplanning import utils, mipPlanner
import random

from mininet.util import makeNumeric

log = core.getLogger()

# TODO: this code is duplicated from mininet/bin/mn, except for TOPOS/topos.
# To fix, extract into a library and make mininet an rpox dependency, or
# extract the topo stuff itself out and make both depend on that shared 
# library.
# MJR: Code from RipL-POX by Brandon Heller
def buildTopo( topo, topos ):
    "Create topology from string with format (object, arg1, arg2,...)."
    topo_split = topo.split( ',' )
    topo_name = topo_split[ 0 ]
    topo_params = topo_split[ 1: ]

    # Convert int and float args; removes the need for every topology to
    # be flexible with input arg formats.
    topo_seq_params = [ s for s in topo_params if '=' not in s ]
    topo_seq_params = [ makeNumeric( s ) for s in topo_seq_params ]
    topo_kw_params = {}
    for s in [ p for p in topo_params if '=' in p ]:
        key, val = s.split( '=' )
        topo_kw_params[ key ] = makeNumeric( val )

    if topo_name not in topos.keys():
        raise Exception( 'Invalid topo_name %s' % topo_name )
    return topos[ topo_name ]( *topo_seq_params, **topo_kw_params )

class Updates(EventMixin):

    def __init__(self, topo):
        if core.hasComponent("openflow"):
            self.listenTo(core.openflow)
        else:
            self.listenTo(core)

        self.topo = topo
        log.info("Topology: " + str(topo.nodes()))

        f1, f2, g = testFlowGen.compute_flows(topo=topo, numEdgeSwitches=4, seed=1)
        self.initial_paths = [e[3] for e in f1]
        self.final_paths = [e[3] for e in f2]
        log.info("final_paths: " + str(self.final_paths))
        log.info("initial_paths: " + str(self.final_paths))
        f1_vec = utils.get_paths_from_flows(f1, g)
        f2_vec = utils.get_paths_from_flows(f2, g)
        e_vec = utils.get_edge_cap_vec(g)

        mp = mipPlanner.Mip_Planner(g)
        mp_obj, schedule = mp.get_plan(f1_vec, f2_vec, e_vec)
        self.schedule = [int(e) for e in schedule]
        self.switchCount = len(topo.switches())
        self.installed = False

    def _handle_ComponentRegistered(self, event):
        self.addListener(GoingDownEvent, _handle_GoingDownEvent)
        if event.name == "openflow":
            self.listenTo(core.openflow)
        else:
            pass

    def install_policy( self, con ):
        for path in self.initial_paths:
            rev_path = [f for f in path]
            rev_path.reverse()
            self.install_path( path, prio=1 )
            self.install_path( rev_path, prio=1 )            

    def install_path( self, path, prio=65535 ):
        rules = self.gen_path_rules(path, prio=prio)
        rules.reverse()
        for con, rule in rules:
            con.send(rule)

    def uninstall_path( self, path, prio=65535 ):
        rules = self.gen_path_rules(path, prio=prio)
        for con, rule in rules:
            rule.command = of.OFPFC_DELETE_STRICT
            con.send(rule)
        
    def gen_path_rules( self, path, prio=None ):
        if not prio:
            prio = 65535
        # core.openflow.getConnection(dpid)
        log.info("Path: " + str(path))
        rules = []
        src = path[0]
        dst = path[-1]
        path = path[1:-1]
        src_ip = of.IPAddr("10.0.0." + src[1:])
        dst_ip = of.IPAddr("10.0.0." + dst[1:])
        prev_hop = src
        cur_hop = path[0]
        path = path[1:]
        for next_hop in path:
            match = of.ofp_match(dl_type=0x800, nw_src=src_ip, nw_dst=dst_ip)
            match.in_port = self.topo.topo.ports[str(cur_hop)][str(prev_hop)]
            flow = of.ofp_flow_mod(match=match)
            flow.idle_timeout = of.OFP_FLOW_PERMANENT
            flow.hard_timeout = of.OFP_FLOW_PERMANENT
            flow.priority = prio
            flow.actions = [ of.ofp_action_output( port=self.topo.topo.ports[str(cur_hop)][str(next_hop)] ) ]
            con = core.openflow.getConnection(cur_hop)
            rules.append((con, flow))
            prev_hop = cur_hop
            cur_hop = next_hop
        # Now need to install final rule to dst host
        next_hop = dst
        match = of.ofp_match(dl_type=0x800, nw_src=src_ip, nw_dst=dst_ip)
        match.in_port = self.topo.topo.ports[str(cur_hop)][str(prev_hop)]
        flow = of.ofp_flow_mod(match=match)
        flow.idle_timeout = of.OFP_FLOW_PERMANENT
        flow.hard_timeout = of.OFP_FLOW_PERMANENT
        flow.priority = prio
        flow.actions = [ of.ofp_action_output( port=self.topo.topo.ports[str(cur_hop)][str(next_hop)] ) ]
        con = core.openflow.getConnection(cur_hop)
        rules.append((con,flow))
        return rules

    def migrate(self):
        log.info("migrating schedule: " + str(self.schedule))
        for p in self.schedule:
            final_path = self.final_paths[p-1]
            rev_final_path = [ e for e in final_path ]
            rev_final_path.reverse()
            self.install_path(final_path, prio=2)
            self.install_path(rev_final_path, prio=2)
        for p in self.schedule:
            initial_path = self.initial_paths[p-1]
            rev_initial_path = [e for e in initial_path ]
            rev_initial_path.reverse()
            self.uninstall_path(initial_path, prio=1)
            self.uninstall_path(rev_initial_path, prio=1)            
        log.info("done migrating!")
                
    # You should modify the handlers below.
    def _handle_ConnectionUp(self, event):
        switch = event.dpid
        self.switchCount -= 1
        if self.switchCount == 0 and not self.installed:
            self.install_policy(event.connection)
            self.installed = True
            core.callDelayed(7, self.migrate)

    def _handle_ConnectionDown(self, event):
        pass

    def _handle_FlowRemoved(self, event):
        pass

    def _handle_PortStatus(self,event):
        pass

    def _handle_FlowStatsReceived(self,event):
        pass

    def _handle_PacketIn(self, event):
        switch = event.dpid
        port = event.port
        packet = event.parsed
        log.info("PacketIn from : " + str(switch) + " " + str(packet))

        return

def launch(topo=None):
    if not topo:
        raise Exception("please specify topo and args on cmd line")

    log.info("Shortest Path running with topo=%s." % topo)
    topo = buildTopo(topo, topos)
    core.registerNew(Updates, topo)
