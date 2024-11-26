# https://ohmyz.sh
export ZSH="$HOME/.oh-my-zsh"
zstyle ':omz:update' mode reminder
zstyle ':omz:update' frequency 30
ZSH_THEME="afowler"
HYPHEN_INSENSITIVE="true"
HIST_STAMPS="mm/dd/yyyy"
plugins=(git asdf z zsh-navigation-tools jump)
source $ZSH/oh-my-zsh.sh

export MANPATH="/usr/local/man:$MANPATH"
export LANG=en_US.UTF-8
export EDITOR="/home/myxi/.local/bin/em"
export SUDO_EDITOR="/home/myxi/.local/bin/kak"
export XDG_CONFIG_HOME="/home/myxi/.config/"
export TERM="xterm-256color"

# Ease of use
alias py="python"
alias nvide="neovide"
alias lg=lazygit
alias trsh="gio trash"

# https://github.com/eeriemyxi/sper
export SPER_LICENSE_DIR="/home/myxi/Documents/licenses"

# https://github.com/jarun/nnn/
# theme (gruvbox)
BLK="08" CHR="0E" DIR="0C" EXE="0A" REG="0F" HARDLINK="0F" SYMLINK="0D" MISSING="09" ORPHAN="09" FIFO="0B" SOCK="0C" OTHER="0F"
export NNN_FCOLORS="$BLK$CHR$DIR$EXE$REG$HARDLINK$SYMLINK$MISSING$ORPHAN$FIFO$SOCK$OTHER"
# plugins
export NNN_PLUG='f:fzplug;n:kak_open'

# Nim
export PATH=~/Documents/tools/nim-2.2.0/bin:$PATH
export PATH=/home/myxi/.nimble/bin:$PATH
alias nimpretty="nimpretty --indent:4"

# Deno
export PATH=~/.asdf/installs/deno/2.1.1/.deno/bin/:$PATH

# Cargo
export PATH=/home/myxi/.cargo/bin:$PATH

# Android
export ANDROID_HOME=$HOME/.android-sdk/Sdk
export PATH=$PATH:$ANDROID_HOME/platform-tools
export GRADLE_USER_HOME=~/.config/gradle
export PATH=/home/myxi/Documents/tools/gradle-8.2.1/bin:$PATH
export PATH=/home/myxi/Documents/tools/android-studio/bin:$PATH

# https://github.com/Aloxaf/fzf-tab
source ~/Documents/tools/fzf-tab/fzf-tab.plugin.zsh

# https://github.com/zdharma-continuum/fast-syntax-highlighting
source ~/Documents/tools/fast-syntax-highlighting/fast-syntax-highlighting.plugin.zsh

# https://vi.stackexchange.com/a/22822
# function my_man {
#     $EDITOR +"Man $1|on"
# }

# Use kak as manpager
function my_man {
    kak -e "man $1"
}
alias man="my_man"

# Personal helper function
function c {
    if [[ $# -eq 0 ]] then
        echo "Usage: c [z [r, e], v [v [p [d]], ]]"
        return
    fi

    case $1 in
        z)
            case $2 in
                r)
                    source ~/.zshrc
                    ;;
                e)
                    $EDITOR ~/.zshrc
                    ;;
            esac
            ;;
        v)
            case $2 in
                p)
                    case $3 in
                        d)
                            deactivate
                            return
                            ;;
                    esac
                    source .venv/bin/activate
                    ;;
            esac
            ;;
        tt)
            case $2 in
                one)
                    command tt -words english -theme gruvbox-personal -t 60 -bold \
                        -showwpm
                    ;;
                two)
                    command tt -words ten-thousand -theme gruvbox-personal -t 60 -bold \
                        -showwpm
                    ;;
            esac
            ;;
    esac
}
