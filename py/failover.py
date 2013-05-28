from mininet.net import Mininet
from mininet.cli import CLI
from mininet.node import Node, UserSwitch, Host, RemoteController
from mininet.link import Link, TCIntf
from mininet.util import custom
from mininet.log import lg
from CustomTopo import Diamond
from datetime import datetime
from time import sleep
import sys

def run_iperf_server( net, srv ):
    cmd = "iperf -s"
    print
    print "*** Starting iperf server on %s " % ( str( srv ) )
    print "*** cmd: " + cmd
    srv.sendCmd( cmd )

def run_iperf_client( net, cli, srv, duration ):
    cmd = "iperf -t %d -c %s" % ( duration, str( srv.IP() ) )
    print
    print "*** Starting iperf client on %s " % ( str( cli ) )
    print "*** cmd: " + cmd
    cli.sendCmd( cmd )
    
def testFailover( net, duration=10 ):
    startTime = datetime.now()
    print "* Starting failover test at " + str( startTime )
    print
    run_iperf_server( net, net.hosts[1] )
    sleep( 1 )
    run_iperf_client( net, net.hosts[0], net.hosts[1], duration )
    
    # FAIL LINK
    sleep( duration / 2.0 )
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
    net.configHosts()
    net.start()
#    start_sshd( net )
    CLI(net)    
    testFailover( net )
#    stop_sshd( net )        
    net.stop()
        
if __name__ == '__main__':
    lg.setLogLevel( 'info')
    failover_test()
