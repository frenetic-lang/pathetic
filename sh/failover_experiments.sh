#!/bin/sh
mkdir ~/failover_results
mkdir ~/failover_results/fail_link_yes
mkdir ~/failover_results/fail_link_no
#for fl in yes no; do
#  for i in `seq 1 10`; do
#    sudo FAIL_LINK=$fl ./run_failover.sh &
#    sleep 5
#    ./run_pathetic.sh
#    sleep 50
#    cp /tmp/iperf-client.log ~/failover_results/fail_link_$fl/iperf-client-$i.log
#  done
#done
for i in `seq 1 10`; do
  for fl in yes no; do
    echo "iteration " ${i} "started"
    sudo FAIL_LINK=$fl ./run_failover.sh &
    MN_PID=$!
    sleep 5
    ./run_pathetic.sh &
    NC_PID=$!
    sleep 50
    kill ${NC_PID}
    wait ${MN_PID}
    cp /tmp/iperf-client.log ~/failover_results/fail_link_$fl/iperf-client-$i.log
    echo "iteration " ${i} "ended"
  done
done
