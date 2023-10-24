#!/bin/sh

run() {
  if ! pgrep -f "$1" ;
  then
    "$@"&
  fi
}

run /usr/bin/picom --fade-in-step=1 --fade-out-step=1 --fade-delta=0 --active-opacity 1 -e 1 -i 1 --inactive-dim 0.1 --shadow-radius 8 -o 0.50
run /usr/bin/node ~/Documents/arRPC/src

run nm-applet
run greenclip daemon
run /usr/lib/kdeconnectd
run easyeffects --gapplication-service

