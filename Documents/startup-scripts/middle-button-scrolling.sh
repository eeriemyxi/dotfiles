#!/bin/bash
# Get the device name from: xinput list

DEVICE_NAME="optical mouse"

device_ids=$(xinput list | grep -i "$DEVICE_NAME" | awk -F'id=' '{print $2}' | awk '{print $1}')

if [ -z "$device_ids" ]; then
    echo "Device not found: $DEVICE_NAME"
    exit 1
fi

export ID=$device_ids

xinput set-prop $ID "libinput Scroll Method Enabled" 0, 0, 1  
xinput set-prop $ID "libinput Button Scrolling Button" 2
