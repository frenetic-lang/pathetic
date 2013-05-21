from mininet.net import Mininet
from mininet.cli import CLI
from mininet.node import Node, UserSwitch, Host, RemoteController
from mininet.link import Link, TCIntf
from mininet.util import custom
from mininet.log import lg
from CustomTopo import *
from datetime import datetime
from multiprocessing import Process, Pool
import testFlowGen
import sys

def getFile( net, src, dst, load ):
    cmd = "sftp -o StrictHostKeyChecking=no -l %d %s:/home/mininet/10.bin /dev/null" % ( load * 1024, str(dst.IP()),  )
    print
    print "*** Starting SFTP of %s mbs from %s to %s " % ( str(load), str(src.name), str(dst) )
    print "*** cmd: " + cmd
    src.sendCmd( cmd )

def testThroughput( net, traffic ):
    startTime = datetime.now()
    print "* Starting throughput test at " + str(startTime)
    print
    # pool = Pool()
    for src,dst,load in traffic:
        # net.get(src).setDefaultRoute()
        getFile( net, net.get(src), net.get(dst), load )
    for src,dst,load in traffic:
        print " output: " + net.get(src).waitOutput(True)
        
    # pool.close()
    # pool.join()
    endTime = datetime.now()
    print
    print "* Ending throughput test at " + str(endTime)
    print "* Total time: " + str(endTime - startTime)

def connectToRootNS( network, switch, routes ):
    """Connect hosts to root namespace via switch. Starts network.
      network: Mininet() network object
      switch: switch to connect to root namespace
      ip: IP address for root namespace node
      prefixLen: IP address prefix length (e.g. 8, 16, 24)
      routes: host networks to route to"""
    # Create a node in root namespace and link to switch 0
    root = Node( 'root', inNamespace=False )
    intf = Link( root, switch ).intf1
    # root.setIP( ip, prefixLen, intf )
    # Start network that now includes link to root namespace
    network.start()
    # Add routes from root ns to hosts
    for route in routes:
        root.cmd( 'route add -net ' + route + ' dev ' + str( intf ) )

def start_sshd( network, cmd='/usr/sbin/sshd', opts='-D' ):
    "Start a network, connect it to root ns, and run sshd on all hosts."
    for host in network.hosts:
        host.cmd( cmd + ' ' + opts + '&' )
    print
    print "*** Hosts are running sshd at the following addresses:"
    print
    for host in network.hosts:
        print host.name, host.IP()
    print

def stop_sshd( network, cmd='/usr/sbin/sshd', opts='-D' ):
    "Start a network, connect it to root ns, and run sshd on all hosts."
    for host in network.hosts:
        host.cmd( 'kill %' + cmd )
        
def limit( traffic, numEdgeSwitches=20, bw=100 ):
    """Example/test of link and CPU bandwidth limits
       traffic: traffic matrix (src,dst,load)
       bw: interface bandwidth limit in Mbps
       cpu: cpu limit as fraction of overall CPU time"""
    # intf = custom( TCIntf, bw=bw, use_tbf=True )
    # host = custom( Host, intf=intf )
    myTopo = FattreeTopology( numEdgeSwitches=numEdgeSwitches )
    net = Mininet( topo=myTopo.mininet_topo(), autoStaticArp=True, \
                   switch=UserSwitch, \
                   controller=RemoteController )
    switch = net.switches[ 0 ]
    routes = [ '10.0.0.0/8' ]  # host networks to route to
    net.configHosts()
    # connectToRootNS( net, switch, routes )
    net.start()
    start_sshd( net )
    CLI(net)    
    testThroughput( net, traffic )
    stop_sshd( net )        
    net.stop()
        
if __name__ == '__main__':
    lg.setLogLevel( 'info')
    f1,f2,g = testFlowGen.compute_flows(topo=FattreeTopology(numEdgeSwitches=4), numEdgeSwitches=4, seed=1)
    traffic = [ (src, dst, bw) for src,dst,bw,_ in f1 ]
    limit( traffic,
        numEdgeSwitches=4 )
