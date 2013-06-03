#!/bin/sh
#export CAML_LD_LIBRARY_PATH=/home/openflow/ocamlbrew/ocaml-4.00.1/.opam/system/lib/stublibs:/home/openflow/ocamlbrew/ocaml-4.00.1/lib/ocaml/stublibs
#export FAIL_LINK=no
python ../py/failover.py
cat /tmp/iperf-client.log
pkill ofdatapath
pkill ofprotocol
pkill iperf
