#!python
"""Import the configuration file of Alacritty Terminal, update the font size as specified, and then exit.
License: https://unlicense.org/
Author: Myxi (github.com/eeriemyxi)
Year: 2023 
"""

import sys

import yaml

USER = "eeriemyxi"


if __name__ == "__main__":
    try:
        font_size = int(sys.argv[1])
    except ValueError:
        raise Exception("That's not a valid value to put as the font.")

    with open(f"/home/{USER}/.config/alacritty/alacritty.yml", "r") as file:
        yml = yaml.safe_load(file.read())
        yml["font"]["size"] = font_size

    with open(f"/home/{USER}/.config/alacritty/alacritty.yml", "w") as file:
        file.write(yaml.dump(yml))
