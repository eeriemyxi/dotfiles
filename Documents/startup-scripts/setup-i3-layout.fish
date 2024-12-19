# function sleep_for_wezterm
#     while test (count xdotool search wezterm 2> /dev/null) -lt $argv[1]
#         sleep 1
#     end
# end
#
# for i in (seq 2)
#     wezterm &
# end
# sleep_for_wezterm 2
# sleep 5
#
# i3 focus left
# i3 split h
#
# wezterm &
# sleep_for_wezterm 3
# sleep 2
#
# i3 focus right
# i3 layout tabbed

i3 exec wezterm
i3 exec wezterm
i3 exec wezterm
i3 exec chromium
i3 exec dissent
# i3 exec spotify

uv run -s /home/myxi/Documents/startup-scripts/i3-smart-titlebar.py &
