fenv source ~/.zprofile

set fish_greeting

alias py "python"
alias nvide "neovide"
alias lg "lazygit"
alias trsh "gio trash"
alias amyxi "distrobox enter amyxi"
alias ain "sudo apt install"
alias aun "sudo apt remove"

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
