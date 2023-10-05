local lyaml = require("lyaml")


local alacritty_config_file_read_buffer = io.open("/home/eeriemyxi/.config/alacritty/alacritty.yml", "r")

io.input(alacritty_config_file_read_buffer)
local alacritty_config_file = io.read("*all")
local yml = lyaml.load(alacritty_config_file)

yml["font"]["size"] = tonumber(arg[1])

local alacritty_config_file_write_buffer = io.open("/home/eeriemyxi/.config/alacritty/alacritty.yml", "w")
io.output(alacritty_config_file_write_buffer)
io.write(lyaml.dump({
  yml
}))
