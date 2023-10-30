local awful = require("awful")
local gears = require("gears")

local constants = require("ext.constants")
local modkey = constants.MOD_KEY

local personal_keys = gears.table.join(
    awful.key({ modkey }, "s", function()
        awful.spawn("rofi -show run")
    end),
    awful.key({ modkey }, "v", function()
        awful.spawn(
            "rofi -modi 'clipboard:greenclip print' "
                .. "-show clipboard -run-command '{cmd}'"
        )
    end),
    awful.key({ modkey, "Shift" }, "d", function()
        awful.spawn("kdocker")
    end),
    awful.key({ modkey, "Shift" }, "l", function()
        awful.spawn("xtrlock -f")
    end),
    awful.key({}, "Print", function()
        awful.spawn("flameshot gui")
    end),

    awful.key({}, "XF86AudioRaiseVolume", function()
        awful.spawn("pamixer -i 10")
    end),
    awful.key({}, "XF86AudioLowerVolume", function()
        awful.spawn("pamixer -d 10")
    end),
    awful.key({}, "XF86AudioMute", function()
        awful.spawn("pactl set-sink-mute @DEFAULT_SINK@ toggle")
    end),
    awful.key({ modkey, "Shift" }, "h", function()
        awful.spawn(
            "setxkbmap us -option shift:both_capslock " .. "-variant colemak; xset r 66"
        )
    end),

    awful.key({}, "XF86AudioPlay", function()
        awful.spawn("playerctl play-pause")
    end),
    awful.key({}, "XF86AudioNext", function()
        awful.spawn("playerctl next")
    end),
    awful.key({}, "XF86AudioPrev", function()
        awful.spawn("playerctl previous")
    end),
    awful.key({}, "XF86AudioStop", function()
        awful.spawn("playerctl stop")
    end),

    -- Got a new keyboard, but it doesn't have 
    -- media keys...
    awful.key({ "Mod1", "Shift" }, "k", function()
        awful.spawn("playerctl play-pause")
    end),
    awful.key({ "Mod1", "Shift" }, "l", function()
        awful.spawn("playerctl previous")
    end),
    awful.key({ "Mod1", "Shift" }, "u", function()
        awful.spawn("playerctl next")
    end)
)

return personal_keys
