#!/bin/bash

##
# This script defines a new key in Redis of type SET and name '_subs:registry' which contains all subscriptions keys.
# To get all subscriptions keys, iterates with a cursor over all keys that match expression subs:*
##

  port=$1
  REDIS_PWD=$2
  REDIS_CMD="/usr/local/bin/redis-cli -p $port -a $REDIS_PWD"

  cursor=-1
  total_keys=0
  keys=""
  SECONDS=0

  while [[ $cursor -ne 0 ]]; do
    if [[ $cursor -eq -1 ]]; then
      cursor=0
    fi


    reply=`$REDIS_CMD SCAN $cursor MATCH subs:* COUNT 10000`
    cursor=`expr "$reply" : '\([0-9]*[0-9 ]\)'`
    keys=($(echo "$reply" | tr ' ' '\n'))
    keysLen=${#keys[@]}
   
    for (( i=1; i<$keysLen; i++ )); do
      ((total_keys++))
      $REDIS_CMD SADD "_subs:registry" ${keys[$i]}
      echo "Added to registry subscription with key ${keys[$i]}"
    done
  done