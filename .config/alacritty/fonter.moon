lyaml = require "lyaml"


alacritty_config_file_read_buffer = io.open "/home/eeriemyxi/.config/alacritty/alacritty.yml", "r"

io.input alacritty_config_file_read_buffer
alacritty_config = io.read "*all"

yml = lyaml.load alacritty_config
yml["font"]["size"] = tonumber(arg[1])

alacritty_config_file_write_buffer = io.open "/home/eeriemyxi/.config/alacritty/alacritty.yml", "w"
io.output alacritty_config_file_write_buffer
io.write lyaml.dump {yml}
