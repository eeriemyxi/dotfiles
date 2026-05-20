fenv source ~/.zprofile

if test -f ~/.fish_profile
    source ~/.fish_profile
end

set -x MANROFFOPT "-c"
set -x MANPAGER "sh -c 'col -bx | bat -l man -p'"

set fish_greeting
set QT_QPA_PLATFORMTHEME "qt5ct"

# A lot of it is yoinked from: 
# https://github.com/CachyOS/cachyos-fish-config/blob/main/cachyos-config.fish

alias py "python"
alias nvide "neovide"
alias lg "lazygit"
alias trsh "gio trash"
alias amyxi "distrobox enter amyxi"
alias lls='eza -al --color=always --group-directories-first --icons' # preferred listing
alias la='eza -a --color=always --group-directories-first --icons'  # all files and dirs
alias ll='eza -l --color=always --group-directories-first --icons'  # long format
alias l.="eza -a | grep -e '^\.'" # show only dotfiles
alias grubup="sudo grub-mkconfig -o /boot/grub/grub.cfg"
alias fixpacman="sudo rm /var/lib/pacman/db.lck"
alias tarnow='tar -acf '
alias untar='tar -zxvf '
alias wget='wget -c '
alias psmem='ps auxf | sort -nr -k 4'
alias psmem10='ps auxf | sort -nr -k 4 | head -10'
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'
alias ......='cd ../../../../..'
alias dir='dir --color=auto'
alias vdir='vdir --color=auto'
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'
alias hw='hwinfo --short'                                   # Hardware Info
alias big="expac -H M '%m\t%n' | sort -h | nl"              # Sort installed packages according to size in MB
alias gitpkg='pacman -Q | grep -i "\-git" | wc -l'          # List amount of -git packages
alias update='sudo pacman -Syu'
alias cleanup='sudo pacman -Rns (pacman -Qtdq)'
alias jctl="journalctl -p 3 -xb"
alias mirror="sudo cachyos-rate-mirrors"
alias rip="expac --timefmt='%Y-%m-%d %T' '%l\t%n %v' | sort | tail -200 | nl" # recent installed pkgs
abbr rm "rm -i"
abbr cdt "cd \$(mktemp -d)"
abbr cvp "source .venv/bin/activate.fish"
abbr windows "sudo efibootmgr -n 0 && reboot"
abbr gcl "git clone --depth 1 --single-branch --recurse-submodules --shallow-submodules"

function history
    builtin history --show-time='%F %T '
end

function backup --argument filename
    cp $filename $filename.bak
end


function mkcd
    mkdir -p $argv[1]
    cd $argv[1]
end

function c
    if test (count $argv) -eq 0
        echo "Usage: c [f [r, e], v [v [p [d]], tt [one, two]]]"
        return
    end

    switch $argv[1]
        case o
            switch $argv[2]
                case cl
                    compressed ~/Videos/OBS/(ls -t ~/Videos/OBS | head -n 1)
                case l
                    copy-files ~/Videos/OBS/(ls -t ~/Videos/OBS | head -n 1)
            end
        case f
            switch $argv[2]
                case r
                    source $HOME/.config/fish/config.fish
                case e
                    $EDITOR $HOME/.config/fish/config.fish
            end
        case v
            switch $argv[2]
                case p
                    switch $argv[3]
                        case d
                            deactivate
                            return
                        case '*'
                            source .venv/bin/activate.fish
                    end
            end
        case tt
            switch $argv[2]
                case one
                    command tt -words english -theme gruvbox-personal -t 60 -bold \
                        -showwpm
                case two
                    command tt -words ten-thousand -theme gruvbox-personal -t 60 -bold \
                        -showwpm
            end
    end
end

function copy-files
    if test (count $argv) -eq 0
        echo "Usage: copy-files file1 [file2 ...]"
        return 1
    end

    set uris
    for f in $argv
        if test -e $f
            set abs (realpath $f)
            set uris $uris file://$abs
        else
            echo "Warning: file not found: $f" >&2
        end
    end

    printf "%s\n" $uris | wl-copy --type text/uri-list
end

function compressed
    if test (count $argv) -eq 0
        echo "Usage: compressed <file>"
        return 1
    end

    echo "Compressing" .. $argv

    if not test -e $argv
        echo "Error: file not found: $argv" >&2
        return
    end
    
    set output_dir $(mktemp -d)
    set output_filename $(tr -dc A-Za-z0-9 </dev/urandom | head -c 13; echo)
    set output_extension (path extension $argv)
    set output_path "$output_dir/$output_filename$output_extension"
    
    ffmpeg -i $argv -c:v libx264 -preset slower -crf 26 -c:a aac -b:a 96k -movflags +faststart $output_path
    
    echo Exported to $output_path

    copy-files $output_path

    echo Copied $output_path to clipboard
end

function add-lc -a link
    cd ~/Documents/coding/cp/leetcode/

    if test -z "$link"
        echo "ERROR: Link not provided."
        echo "USAGE: add-lc <link>"
        cd -
        return 1
    end

    if not test -e "./gen"
        echo "ERROR: ./gen executable doesn't exist."
        cd -1
        return 1
    end
    
    set --local paste (wl-paste | string collect)

    if not printf "%s" "$paste" | grep -qe "Solution"
        echo "ERROR: 'Solution' not found in the clipboard data:"
        printf "%s" "$paste"
        cd -
        return 1
    end

    printf "%s" "$paste" | ./gen -i $link
    git add .

    cd -
end

# https://github.com/rexim/tore
if status is-interactive
    if type -q tore
        tore
    end
end

fish_add_path /home/myxi/.spicetify

set -q GHCUP_INSTALL_BASE_PREFIX[1]; or set GHCUP_INSTALL_BASE_PREFIX $HOME ; set -gx PATH $HOME/.cabal/bin /home/myxi/.ghcup/bin $PATH # ghcup-env