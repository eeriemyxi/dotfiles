# pip install black
hook global BufSetOption filetype=python %{
    set-option buffer formatcmd 'black -'
}

# https://deno.com
hook global BufSetOption filetype=javascript %{
    set-option buffer formatcmd "deno fmt -"
}
hook global BufSetOption filetype=yaml %{
    set-option buffer formatcmd 'deno fmt -'
}

# install python3?
hook global BufSetOption filetype=json %{
    set-option buffer formatcmd 'python3 -m json.tool'
}

# https://ziglang.org
hook global BufSetOption filetype=zig %{
    set-option buffer formatcmd 'zig fmt --stdin'
}

# sudo apt install astyle
# sudo apt install clang-format
hook global BufSetOption filetype=cpp %{
    set-option buffer formatcmd 'clang-format --style "{ColumnLimit: 89, IndentWidth: 4}"'
    # set-option buffer formatcmd 'astyle'
}
hook global BufSetOption filetype=c %{
    set-option buffer formatcmd 'clang-format --style "{ColumnLimit: 89, IndentWidth: 4}"'
    # set-option buffer formatcmd 'astyle'
}
