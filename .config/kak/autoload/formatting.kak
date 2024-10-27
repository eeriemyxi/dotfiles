# pip install black
hook global BufSetOption filetype=python %{
    set-option buffer formatcmd 'black -'
}

# https://deno.com
hook global BufSetOption filetype=javascript %{
    set-option buffer formatcmd "deno fmt -"
}

hook global BufSetOption filetype=json %{
    set-option buffer formatcmd 'python3 -m json.tool'
}

hook global BufSetOption filetype=zig %{
    set-option buffer formatcmd 'zig fmt --stdin'
}

hook global BufSetOption filetype=yaml %{
    set-option buffer formatcmd 'deno fmt -'
}

# sudo apt install astyle

hook global BufSetOption filetype=cpp %{
    set-option buffer formatcmd 'astyle'
}

hook global BufSetOption filetype=c %{
    set-option buffer formatcmd 'astyle'
}
