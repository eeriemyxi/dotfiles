# https://ohmyz.sh
export ZSH="$HOME/.oh-my-zsh"
zstyle ':omz:update' mode reminder
zstyle ':omz:update' frequency 30
ZSH_THEME="afowler"
HYPHEN_INSENSITIVE="true"
HIST_STAMPS="mm/dd/yyyy"
DISABLE_MAGIC_FUNCTIONS=true
plugins=(git asdf z)
source $ZSH/oh-my-zsh.sh

# Ease of use
alias py="python"
alias nvide="neovide"
alias lg=lazygit
alias trsh="gio trash"
alias nimpretty="nimpretty --indent:4"

# https://github.com/Aloxaf/fzf-tab
# source ~/Documents/tools/fzf-tab/fzf-tab.plugin.zsh

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

if [ -e /home/myxi/.nix-profile/etc/profile.d/nix.sh ]; then . /home/myxi/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
