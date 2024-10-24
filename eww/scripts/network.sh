#!/usr/bin/env bash

COUNT=0
ADAPTER_NAME="$(ip link | awk '/state UP/ { print $2 }')"

wan_ok () {
    if (ping -c 1 8.8.8.8 || ping -c 1 1.1.1.1) &>/dev/null; then
        echo "o"    
    else
        echo "x"
    fi
}

wan_ok



