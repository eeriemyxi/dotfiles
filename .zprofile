export MANPATH="/usr/local/man:$MANPATH"
export LANG=en_US.UTF-8
export EDITOR="$HOME/.local/bin/em"
export SUDO_EDITOR="$HOME/.local/bin/nvim"
export XDG_CONFIG_HOME="$HOME/.config/"
export TERM="xterm-256color"

# https://github.com/asdf-vm/asdf
export PATH=$HOME/.asdf/shims:$PATH
export PATH=$HOME/.asdf/bin:$PATH

# https://github.com/eeriemyxi/sper
export SPER_LICENSE_DIR="$HOME/Documents/licenses"

# https://github.com/jarun/nnn/
# theme (gruvbox)
BLK="08" CHR="0E" DIR="0C" EXE="0A" REG="0F" HARDLINK="0F" SYMLINK="0D" MISSING="09" ORPHAN="09" FIFO="0B" SOCK="0C" OTHER="0F"
export NNN_FCOLORS="$BLK$CHR$DIR$EXE$REG$HARDLINK$SYMLINK$MISSING$ORPHAN$FIFO$SOCK$OTHER"
export NNN_PLUG='f:fzplug;n:kak_open'

# Distrobox
export PATH=$HOME/.distrobox/bin:$PATH

# Nim
export PATH=$HOME/Documents/tools/nim-2.2.0/bin:$PATH
export PATH=$HOME/.nimble/bin:$PATH

# Deno
export PATH=$HOME/.asdf/installs/deno/2.1.1/.deno/bin/:$PATH

# Cargo
export PATH=$HOME/.cargo/bin:$PATH

# Android
export ANDROID_HOME=$HOME/.android-sdk/Sdk
export PATH=$PATH:$ANDROID_HOME/platform-tools
export GRADLE_USER_HOME=$HOME/.config/gradle
export PATH=$HOME/Documents/tools/gradle-8.2.1/bin:$PATH
export PATH=$HOME/Documents/tools/android-studio/bin:$PATH
