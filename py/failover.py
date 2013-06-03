from mininet.net import Mininet
from mininet.cli import CLI
from mininet.node import Node, UserSwitch, Host, Controller, RemoteController
from mininet.moduledeps import pathCheck
from mininet.link import Link, TCIntf
from mininet.util import custom
from mininet.log import lg
from CustomTopo import Diamond
from datetime import datetime
from time import sleep
import sys
import os

class PatheticController( Controller ):
    def __init__(self, name):
        pdir = "/home/openflow/pathetic/ocaml"
        command = pdir + "/Main_OpenFlow0x04.byte"
        Controller.__init__( self, name, command=command, cargs='', cdir=pdir )

    def start( self ):
        pass

    def real_start( self ):
        """Start <controller> <args> on controller.
           Log to /tmp/cN.log"""
        pathCheck( self.command )
        cout = '/tmp/' + self.name + '.log'
        if self.cdir is not None:
            self.cmd( 'cd ' + self.cdir )
        self.cmd( self.command +
                  ' 1>' + cout + ' 2>' + cout + '&' )
        self.execed = False

def run_iperf_server( net, srv ):
    cmd = "iperf -s > /dev/null 2>&1"
    print
    print "*** Starting iperf server on %s " % ( str( srv ) )
    print "*** cmd: " + cmd
    srv.sendCmd( cmd )

def run_iperf_client( net, cli, srv, duration, volume ):
    if volume == 0:
        cmd = "iperf -t %d -c %s > /tmp/iperf-client.log 2>&1" % ( duration, str( srv.IP() ) )
    else:
        cmd = "iperf -n %dM -c %s > /tmp/iperf-client.log 2>&1" % ( volume, str( srv.IP() ) )
    print
    print "*** Starting iperf client on %s " % ( str( cli ) )
    print "*** cmd: " + cmd
    cli.sendCmd( cmd )
    
def testFailover( net, fail_link='yes', failat=20, duration=60, volume=100 ):
    startTime = datetime.now()
    print "* Starting failover test at " + str( startTime )
    print
    run_iperf_server( net, net.hosts[1] )
    sleep( 1 )
    run_iperf_client( net, net.hosts[0], net.hosts[1], duration, volume )
    
    # FAIL LINK
    if 'FAIL_LINK' in os.environ:
        fail_link = os.environ[ 'FAIL_LINK' ]
    if fail_link == "yes":
        sleep( failat )
        failTime = datetime.now()
        print "* Failing link at " + str( failTime )
        print
        net.configLinkStatus( "s1", "s3", "down" )
    
    output = net.hosts[0].waitOutput( True )
    print " output: " + output
    endTime = datetime.now()
    print
    print "* Ending failover test at " + str( endTime )
    print "* Total time: " + str( endTime - startTime )

def failover_test():
    """Example/test of fast failover"""
    myTopo = Diamond()
    net = Mininet( topo=myTopo, autoStaticArp=True, \
                   switch=UserSwitch, \
                   controller=RemoteController )
                   #controller=PatheticController )
    net.configHosts()
    net.start()
    
    #net.controllers[0].real_start()
    print "* Sleep a few seconds while pathetic installs rules"
    sleep(10)
    #CLI(net)    
    testFailover( net )
    net.stop()

if __name__ == '__main__':
    lg.setLogLevel( 'info')
    failover_test()
