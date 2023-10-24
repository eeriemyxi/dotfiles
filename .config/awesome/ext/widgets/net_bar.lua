local wibox = require("wibox")
local gears = require("gears")
local awful = require("awful")

local constants = require("ext.constants")

local net_bar = wibox.widget({
    {
        id = "identity",
        markup = "  <span color=\"#8ec07c\">net</span> ",
        widget = wibox.widget.textbox,
    },
    {
        id = "usage",
        text = "0 up 0 down",
        widget = wibox.widget.textbox,
    },
    layout = wibox.layout.align.horizontal,
    set_net_usage = function(self, val)
        t = val:sub(1, val:find("\n") - 1)
        bytes_sent = tonumber(t:sub(1, t:find(" ") - 1))
        bytes_recv = tonumber(t:sub(t:find(" ") + 1, t:len()))
        self.usage.text =
            string.format("%s up %s down", bytes_recv // 1024, bytes_sent // 1024)
    end,
})

gears.timer({
    timeout = 1,
    call_now = true,
    autostart = true,
    callback = function()
        awful.spawn.easy_async(constants.SH_CMD_NET_USAGE, function(out)
            net_bar.net_usage = out
        end)
    end,
})

return net_bar
