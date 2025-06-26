local wezterm = require 'wezterm'
local config = wezterm.config_builder()

config.color_scheme = 'Gruvbox Dark (Gogh)'
config.font = wezterm.font 'Maple Mono NF'
config.font_size = 13
config.cell_width = 0.9
config.enable_tab_bar = false
config.default_cursor_style = "SteadyBar"
config.default_prog = { '/usr/bin/fish', '-l' }
config.enable_wayland = true -- doesn't work on hyprland when true

return config
