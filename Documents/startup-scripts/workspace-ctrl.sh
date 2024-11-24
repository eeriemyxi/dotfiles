#!/bin/bash

notify-send "Opening startup apps. Wait."
wmctrl -s 0

wezterm &
wezterm &

sleep 5

for prog in $(xdotool search --sync wezterm 2> /dev/null); do
    wmctrl -i -r "$prog" -t 0
done

wmctrl -s 1
firefox &
sleep 5
wmctrl -i -r $(xdotool search --sync "Mozilla Firefox" 2> /dev/null) -t 1

wmctrl -s 2
spotify &
sleep 5
wmctrl -i -r $(xdotool search --sync "Spotify Free" 2> /dev/null) -t 2

wmctrl -s 0
notify-send "Opening statup apps complete."
