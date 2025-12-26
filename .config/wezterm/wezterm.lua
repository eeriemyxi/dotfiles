local wezterm = require 'wezterm'
local act = wezterm.action
local config = wezterm.config_builder()

config.color_scheme = 'Gruvbox Dark (Gogh)'
config.font = wezterm.font 'Maple Mono NF'
config.font_size = 13
config.cell_width = 0.9
config.enable_tab_bar = false
config.default_cursor_style = "SteadyBar"
config.default_prog = { '/usr/bin/fish', '-l' }
config.enable_wayland = true -- doesn't work on hyprland when true

config.key_tables = {
   copy_mode = {
      { key = 'Tab', mods = 'NONE', action = act.CopyMode 'MoveForwardWord' },
      { key = 'Tab', mods = 'SHIFT', action = act.CopyMode 'MoveBackwardWord' },
      { key = 'Enter', mods = 'NONE', action = act.CopyMode 'MoveToStartOfNextLine' },
      { key = 'Escape', mods = 'NONE', action = act.CopyMode 'Close' },
      { key = 'Space', mods = 'NONE', action = act.CopyMode{ SetSelectionMode =  'Cell' } },
      { key = '$', mods = 'NONE', action = act.CopyMode 'MoveToEndOfLineContent' },
      { key = '$', mods = 'SHIFT', action = act.CopyMode 'MoveToEndOfLineContent' },
      { key = ',', mods = 'NONE', action = act.CopyMode 'JumpReverse' },
      { key = '0', mods = 'NONE', action = act.CopyMode 'MoveToStartOfLine' },
      { key = 'o', mods = 'NONE', action = act.CopyMode 'JumpAgain' },  -- ; → o
      { key = 't', mods = 'NONE', action = act.CopyMode{ JumpBackward = { prev_char = false } } },  -- F → t
      { key = 't', mods = 'SHIFT', action = act.CopyMode{ JumpBackward = { prev_char = false } } },  -- F → t
      { key = 'd', mods = 'NONE', action = act.CopyMode 'MoveToScrollbackBottom' },  -- G → d
      { key = 'd', mods = 'SHIFT', action = act.CopyMode 'MoveToScrollbackBottom' },  -- G → d
      { key = 'h', mods = 'NONE', action = act.CopyMode 'MoveToViewportTop' },
      { key = 'h', mods = 'SHIFT', action = act.CopyMode 'MoveToViewportTop' },
      { key = 'i', mods = 'NONE', action = act.CopyMode 'MoveToViewportBottom' },  -- L → i
      { key = 'i', mods = 'SHIFT', action = act.CopyMode 'MoveToViewportBottom' },  -- L → i
      { key = 'm', mods = 'NONE', action = act.CopyMode 'MoveToViewportMiddle' },
      { key = 'm', mods = 'SHIFT', action = act.CopyMode 'MoveToViewportMiddle' },
      { key = 'y', mods = 'NONE', action = act.CopyMode 'MoveToSelectionOtherEndHoriz' },  -- O → y
      { key = 'y', mods = 'SHIFT', action = act.CopyMode 'MoveToSelectionOtherEndHoriz' },  -- O → y
      { key = 'g', mods = 'NONE', action = act.CopyMode{ JumpBackward = { prev_char = true } } },  -- T → g
      { key = 'g', mods = 'SHIFT', action = act.CopyMode{ JumpBackward = { prev_char = true } } },  -- T → g
      { key = 'v', mods = 'NONE', action = act.CopyMode{ SetSelectionMode =  'Line' } },
      { key = 'v', mods = 'SHIFT', action = act.CopyMode{ SetSelectionMode =  'Line' } },
      { key = '^', mods = 'NONE', action = act.CopyMode 'MoveToStartOfLineContent' },
      { key = '^', mods = 'SHIFT', action = act.CopyMode 'MoveToStartOfLineContent' },
      { key = 'b', mods = 'NONE', action = act.CopyMode 'MoveBackwardWord' },
      { key = 'b', mods = 'ALT', action = act.CopyMode 'MoveBackwardWord' },
      { key = 'b', mods = 'CTRL', action = act.CopyMode 'PageUp' },
      { key = 'c', mods = 'CTRL', action = act.CopyMode 'Close' },
      { key = 's', mods = 'CTRL', action = act.CopyMode{ MoveByPage = (0.5) } },  -- d → s
      { key = 'f', mods = 'NONE', action = act.CopyMode 'MoveForwardWordEnd' },  -- e → f
      { key = 't', mods = 'NONE', action = act.CopyMode{ JumpForward = { prev_char = false } } },  -- f → t
      { key = 't', mods = 'ALT', action = act.CopyMode 'MoveForwardWord' },  -- f → t
      { key = 't', mods = 'CTRL', action = act.CopyMode 'PageDown' },  -- f → t
      { key = 'd', mods = 'NONE', action = act.CopyMode 'MoveToScrollbackTop' },  -- g → d
      { key = 'd', mods = 'CTRL', action = act.CopyMode 'Close' },  -- g → d
      { key = 'h', mods = 'NONE', action = act.CopyMode 'MoveLeft' },
      { key = 'n', mods = 'NONE', action = act.CopyMode 'MoveDown' },  -- j → n
      { key = 'e', mods = 'NONE', action = act.CopyMode 'MoveUp' },  -- k → e
      { key = 'i', mods = 'NONE', action = act.CopyMode 'MoveRight' },  -- l → i
      { key = 'm', mods = 'ALT', action = act.CopyMode 'MoveToStartOfLineContent' },
      { key = 'y', mods = 'NONE', action = act.CopyMode 'MoveToSelectionOtherEnd' },  -- o → y
      { key = 'q', mods = 'NONE', action = act.CopyMode 'Close' },
      { key = 'g', mods = 'NONE', action = act.CopyMode{ JumpForward = { prev_char = true } } },  -- t → g
      { key = 'l', mods = 'CTRL', action = act.CopyMode{ MoveByPage = (-0.5) } },  -- u → l
      { key = 'v', mods = 'NONE', action = act.CopyMode{ SetSelectionMode =  'Cell' } },
      { key = 'v', mods = 'CTRL', action = act.CopyMode{ SetSelectionMode =  'Block' } },
      { key = 'w', mods = 'NONE', action = act.CopyMode 'MoveForwardWord' },
      { key = 'j', mods = 'NONE', action = act.Multiple{ { CopyTo =  'ClipboardAndPrimarySelection' }, { CopyMode =  'Close' } } },  -- y → j
      { key = 'PageUp', mods = 'NONE', action = act.CopyMode 'PageUp' },
      { key = 'PageDown', mods = 'NONE', action = act.CopyMode 'PageDown' },
      { key = 'End', mods = 'NONE', action = act.CopyMode 'MoveToEndOfLineContent' },
      { key = 'Home', mods = 'NONE', action = act.CopyMode 'MoveToStartOfLine' },
      { key = 'LeftArrow', mods = 'NONE', action = act.CopyMode 'MoveLeft' },
      { key = 'LeftArrow', mods = 'ALT', action = act.CopyMode 'MoveBackwardWord' },
      { key = 'RightArrow', mods = 'NONE', action = act.CopyMode 'MoveRight' },
      { key = 'RightArrow', mods = 'ALT', action = act.CopyMode 'MoveForwardWord' },
      { key = 'UpArrow', mods = 'NONE', action = act.CopyMode 'MoveUp' },
      { key = 'DownArrow', mods = 'NONE', action = act.CopyMode 'MoveDown' },
   },
}

return config
