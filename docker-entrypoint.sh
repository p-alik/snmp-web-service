#!/usr/bin/env bash

ARG_TIMEOUT=
if [[ ! -z $SNMP_WS_TIMEOUT ]]
then
  ARG_TIMEOUT="--timeout $SNMP_WS_TIMEOUT"
fi

ARG_RETRY=
if [[ ! -z $SNMP_WS_RETRY ]]
then
  ARG_RETRY="--retry $SNMP_WS_RETRY"
fi

snmp-web-service-exe $ARG_TIMEOUT $ARG_RETRY --readonly-community $SNMP_WS_RO_COMMUNITY
