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
export EDITOR="nvim"
export SUDO_EDITOR="nvim"
export XDG_CONFIG_HOME="/home/myxi/.config/"
export TERM="xterm-256color"

# ease of use aliases
alias py="python"
alias poe="poetry"
alias nvide="neovide"
alias lg=lazygit
alias trsh="gio trash"
alias vi=nvim
alias vim=nvim
alias zshrc="$EDITOR $HOME/.zshrc"

# NNN file manager 
# theme (gruvbox)
BLK="08" CHR="0E" DIR="0C" EXE="0A" REG="0F" HARDLINK="0F" SYMLINK="0D" MISSING="09" ORPHAN="09" FIFO="0B" SOCK="0C" OTHER="0F"
export NNN_FCOLORS="$BLK$CHR$DIR$EXE$REG$HARDLINK$SYMLINK$MISSING$ORPHAN$FIFO$SOCK$OTHER"
# plugins
export NNN_PLUG='f:fzplug;k:kak_open'

# Nim
export PATH=~/Documents/tools/nim-2.2.0/bin:$PATH
export PATH=/home/myxi/.nimble/bin:$PATH
alias nimpretty="nimpretty --indent:4"

# # Bun
# export BUN_INSTALL="$HOME/.bun"
# export PATH="$BUN_INSTALL/bin:$PATH"
# [ -s "/home/myxi/.bun/_bun" ] && source "/home/myxi/.bun/_bun"

# Deno
export PATH=~/.asdf/installs/deno/2.0.2/.deno/bin/:$PATH

# Cargo
export PATH=/home/myxi/.cargo/bin:$PATH

# Android
export ANDROID_HOME=$HOME/.android-sdk/Sdk
export PATH=$PATH:$ANDROID_HOME/platform-tools
export GRADLE_USER_HOME=~/.config/gradle
export PATH=/home/myxi/Documents/tools/gradle-8.2.1/bin:$PATH
export PATH=/home/myxi/Documents/tools/android-studio/bin:$PATH

# fzf-powered tab completions
# https://github.com/Aloxaf/fzf-tab
source ~/Documents/tools/fzf-tab/fzf-tab.plugin.zsh

# syntax highlighting like fish-shell 
# https://github.com/zdharma-continuum/fast-syntax-highlighting
# source /home/myxi/Documents/tools/fast-syntax-highlighting/fast-syntax-highlighting.plugin.zsh
source ~/Documents/tools/fast-syntax-highlighting/fast-syntax-highlighting.plugin.zsh

# use nvim as manpager: https://vi.stackexchange.com/a/22822
function my_man {
    $EDITOR +"Man $1|on"
}
alias man="my_man"

# personal helper function
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
