function sleep_for_wezterm
    while test (count xdotool search wezterm 2> /dev/null) -lt $argv[1]
        sleep 1
    end
end

for i in (seq 2)
    wezterm &
end
sleep_for_wezterm 2
sleep 5

i3 focus left
i3 split v

wezterm &
sleep_for_wezterm 3
sleep 2

i3 focus right

i3 exec firefox
i3 exec spotify
