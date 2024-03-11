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
export PATH=~/.local/bin/platform-tools:$PATH

alias zshconfig="$EDITOR ~/.zshrc"
alias zshconfigref=". ~/.zshrc"
alias py="python"
alias poe="poetry"
alias nvide="neovide"
alias venv="source .venv/bin/activate"

# Nim
export PATH=~/Documents/tools/nim-2.0.2/bin:$PATH

# Bun
export BUN_INSTALL="$HOME/.bun"
export PATH="$BUN_INSTALL/bin:$PATH"
[ -s "/home/myxi/.bun/_bun" ] && source "/home/myxi/.bun/_bun"

# Android
export ANDROID_HOME=$HOME/.android-sdk/Sdk
export PATH=$PATH:$ANDROID_HOME/platform-tools

autoload -U compinit; compinit

# fzf-powered tab completions
source ~/Documents/tools/fzf-tab/fzf-tab.plugin.zsh

# syntax highlighting like fish-shell 
ZSH_HIGHLIGHT_HIGHLIGHTERS+=(brackets pattern cursor)
typeset -A ZSH_HIGHLIGHT_STYLES
ZSH_HIGHLIGHT_STYLES[command]='fg=yellow,bold'
ZSH_HIGHLIGHT_STYLES[alias]='fg=yellow,bold'
source ~/Documents/tools/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
