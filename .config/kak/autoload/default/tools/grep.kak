declare-option -docstring "shell command run to search for subtext in a file/directory" \
    str grepcmd 'grep -RHn'

provide-module grep %{

require-module jump

define-command -params .. -docstring %{
    grep [<arguments>]: grep utility wrapper
    All optional arguments are forwarded to the grep utility
    Passing no argument will perform a literal-string grep for the current selection
} grep %{ evaluate-commands %sh{
    if [ $# -eq 0 ]; then
        case "$kak_opt_grepcmd" in
        ag\ * | git\ grep\ * | grep\ * | rg\ * | ripgrep\ * | ugrep\ * | ug\ *)
            set -- -F "${kak_selection}"
            ;;
        ack\ *)
            set -- -Q "${kak_selection}"
            ;;
        *)
            set -- "${kak_selection}"
            ;;
        esac
    fi

     output=$(mktemp -d "${TMPDIR:-/tmp}"/kak-grep.XXXXXXXX)/fifo
     mkfifo ${output}
     ( { trap - INT QUIT; ${kak_opt_grepcmd} "$@" 2>&1 | tr -d '\r'; } > ${output} 2>&1 & ) > /dev/null 2>&1 < /dev/null

     printf %s\\n "evaluate-commands -try-client '$kak_opt_toolsclient' %{
               edit! -fifo ${output} *grep*
               set-option buffer filetype grep
               set-option buffer jump_current_line 0
               hook -always -once buffer BufCloseFifo .* %{ nop %sh{ rm -r $(dirname ${output}) } }
           }"
}}
complete-command grep file 

hook -group grep-highlight global WinSetOption filetype=grep %{
    add-highlighter window/grep group
    add-highlighter window/grep/ regex "^([^:\n]+):(\d+):(\d+)?" 1:cyan 2:green 3:green
    add-highlighter window/grep/ line %{%opt{jump_current_line}} default+b
    hook -once -always window WinSetOption filetype=.* %{ remove-highlighter window/grep }
}

hook global WinSetOption filetype=grep %{
    hook buffer -group grep-hooks NormalKey <ret> jump
    hook -once -always window WinSetOption filetype=.* %{ remove-hooks buffer grep-hooks }
}

define-command -hidden grep-jump %{
    jump
}

define-command grep-next-match -docstring %{alias for "jump-next *grep*"} %{
    jump-next -matching \*grep(-.*)?\*
}

define-command grep-previous-match -docstring %{alias for "jump-previous *grep*"} %{
    jump-previous -matching \*grep(-.*)?\*
}

}

hook -once global KakBegin .* %{ require-module grep }
