-- Pull in the wezterm API
local wezterm = require 'wezterm'

-- This will hold the configuration.
local config = wezterm.config_builder()

-- This is where you actually apply your config choices

-- For example, changing the color scheme:
config.color_scheme = 'Gruvbox Dark (Gogh)'
config.font_size = 10
config.enable_tab_bar = false
config.default_cursor_style = "SteadyBar"
config.default_prog = { '/usr/bin/fish', '-l' }
-- config.enable_csi_u_key_encoding = true

-- and finally, return the configuration to wezterm
return config
