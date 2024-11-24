#!/bin/bash

WAIT_TIME=5

until pgrep -i "pulseeffects"
do
    echo "Waiting for Pulse Effects to open..."
    sleep 1
done

# notify-send "Fixing Pulse Effects after $WAIT_TIME seconds."

sleep $WAIT_TIME

pulseeffects -l reset
# pulseeffects -l default
# pulseeffects -l probuds

# notify-send "Done fixing Pulse Effects."
