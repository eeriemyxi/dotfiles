local wibox = require("wibox")
local gears = require("gears")
local awful = require("awful")

local constants = require("ext.constants")

local cpu_bar = wibox.widget({
    {
        id = "identity",
        markup = "  <span color=\"#83a598\">cpu</span> ",
        widget = wibox.widget.textbox,
    },
    {
        id = "usage",
        text = "0%",
        widget = wibox.widget.textbox,
    },
    layout = wibox.layout.align.horizontal,
    set_cpu_usage = function(self, val)
        self.usage.text = val:gsub("%s+", "") .. "%"
    end,
})

gears.timer({
    timeout = 3,
    call_now = true,
    autostart = true,
    callback = function()
        awful.spawn.easy_async(constants.SH_CMD_CPU_USAGE, function(out)
            cpu_bar.cpu_usage = out
        end)
    end,
})

return cpu_bar
