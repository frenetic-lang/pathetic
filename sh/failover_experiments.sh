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
    sudo FAIL_LINK=$fl ./run_failover.sh &
    sleep 5
    ./run_pathetic.sh
    sleep 50
    cp /tmp/iperf-client.log ~/failover_results/fail_link_$fl/iperf-client-$i.log
  done
done
