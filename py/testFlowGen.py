import random
from CustomTopo import *

def subtract_path(bw_topo, path, bw):
    iterator = path.__iter__()
    s = iterator.next()
    for t in iterator:
        bw_topo[s][t]['bw'] = bw_topo[s][t]['bw'] - bw
        # assert bw_topo[s][t]['bw'] >= 0, "edge (%r,%r) < 0: %r" % (s,t, bw_topo[s][t]['bw'])
        if bw_topo[s][t]['bw'] <= 0:
            bw_topo.remove_edge(s,t)
        s = t

def compute_flows(topo=None, numEdgeSwitches=20, seed=None):

    if not topo:
        topo = FattreeTopology(numEdgeSwitches)

    if not seed:
        import time
        seed = int(time.time())
    # print "Seed:", seed
    random.seed(seed)

    """ Going to model three different applications: High (10), Med
    (5), Low (1) BW requirements. Randomly assign 1/4 hosts to be
    servers. Randomly assign applications,servers to each of the
    remaining hosts.

    To model BW, I'll create a copy of the graph and annotate each
    edge w/ a fixed BW (say 100). Every time I route an application,
    I'll subtract the required BW from each edge.

    Then, to model shifting demand, I'll randomly upgrade/downgrade
    some of the applications and reroute.
    """

    apps = [10,20,30]

    servers = random.sample(topo.hosts(), len(topo.hosts()) / 4)

    clients = [h for h in topo.hosts() if h not in servers]

    # randomly assign apps and hosts
    assignments = [(c, random.choice(apps), random.choice(servers))
                   for c in clients]

    for u,v in topo.edges_iter(): topo[u][v]['bw']=100
        
    bw_graph = topo.nx_graph()

    flows1 = []
    for client,bw,server in assignments:
        route_graph = bw_graph.copy()
        (route_graph.remove_edge(u,v) for u,v,data in
         route_graph.edges_iter(data=True) if data['bw'] < bw)
        
        paths = [p for p in nx.all_shortest_paths(route_graph, client, server)]
        path = random.choice(paths)
        
        flows1.append((client,server,bw,path))
        subtract_path(bw_graph, path, bw)

    bw_graph = topo.nx_graph()

    flows2 = []
    for client,bw,server in assignments:
        #bw = random.choice(apps + [bw])
        route_graph = bw_graph.copy()
        (route_graph.remove_edge(u,v) for u,v,data in
         route_graph.edges_iter(data=True) if data['bw'] < bw)
        
        paths = [p for p in nx.all_shortest_paths(route_graph, client, server)]
        path = random.choice(paths)
        
        flows2.append((client,server,bw,path))
        subtract_path(bw_graph, path, bw)
        
    return (flows1, flows2, topo)
