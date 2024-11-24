evaluate-commands %sh{
    bg0="rgb:282828"
    bg0_hard="rgb:1d2021"
    bg0_soft="rgb:32302f"

    bg1="rgb:3c3836"
    bg2="rgb:504945"
    bg3="rgb:665c54"
    bg4="rgb:7c6f64"

    fg0="rgb:fbf1c7"
    fg0_hard="rgb:f9f5d7"
    fg0_soft="rgb:f2e5bc"

    fg1="rgb:ebdbb2"
    fg2="rgb:d5c4a1"
    fg3="rgb:bdae93"
    fg4="rgb:a89984"

    # Bright Colors
    b_red="rgb:fb4934"
    b_green="rgb:b8bb26"
    b_yellow="rgb:fabd2f"
    b_blue="rgb:83a598"
    b_purple="rgb:d3869b"
    b_aqua="rgb:8ec07c"
    b_orange="rgb:fe8019"
    b_gray="rgb:928374"

    # Normal Colors
    red="rgb:cc241d"
    green="rgb:98971a"
    yellow="rgb:d79921"
    blue="rgb:458588"
    purple="rgb:b16286"
    aqua="rgb:689d6a"
    orange="rgb:d65d0e"

    # Faded Colors
    f_red="rgb:9d0006"
    f_green="rgb:79740e"
    f_yellow="rgb:b57614"
    f_blue="rgb:076678"
    f_purple="rgb:8f3f71"
    f_aqua="rgb:427b58"
    f_orange="rgb:af3a03"
    f_gray="rgb:828374"

    echo "
        # Code highlighting
        face global value           ${b_purple}
        face global type            ${b_yellow}
        face global variable        ${b_blue}
        face global module          ${b_yellow}
        face global function        ${b_green}+b
        face global string          ${b_green}+i
        face global keyword         ${b_red}
        face global operator        ${b_blue}
        face global attribute       ${b_orange}
        face global comment         ${b_gray}+ai
        face global documentation   ${fg3}
        face global meta            ${b_aqua}
        face global builtin         ${b_yellow}+b

        # Markup
        face global title           ${b_green}+b
        face global header          ${b_blue}+b
        face global bold            ${fg0},${fg0}+b
        face global italic          ${fg0},${fg0}+i
        face global mono            ${fg2}
        face global block           ${fg3}
        face global link            ${b_purple}+u
        face global bullet          ${b_aqua}
        face global list            ${fg0}

        # Built-in interface elements
        face global Default            ${fg0},${bg0}
        face global PrimarySelection   ${fg1},${bg2}
        face global SecondarySelection ${fg1},${bg1}
        face global PrimaryCursor      ${bg0},${fg0}+fg
        face global SecondaryCursor    ${bg0},${fg3}+fg
        face global PrimaryCursorEol   ${bg0},${fg2}+fg
        face global SecondaryCursorEol ${bg0},${fg4}+fg
        face global LineNumbers        ${fg3},${bg0}
        face global LineNumberCursor   ${b_yellow},${bg1}+b
        face global LineNumbersWrapped ${fg1},${bg0}
        face global MenuForeground     ${fg1},${bg3}+b
        face global MenuBackground     ${fg1},${bg2}
        face global MenuInfo           ${b_blue},${bg1}
        face global Information        ${fg4},${bg1}
        face global Error              ${b_red},${bg0}+b
        face global DiagnosticError    ${b_red},${bg0}+b
        face global DiagnosticWarning  ${b_yellow},${bg1}+b
        face global StatusLine         ${fg1},${bg1}
        face global StatusLineMode     ${fg1},${bg3}
        face global StatusLineInfo     ${fg3},${bg1}
        face global StatusLineValue    ${b_red},${bg1}
        face global StatusCursor       ${bg0},${fg0}
        face global Prompt             ${fg1},${bg1}+b
        face global MatchingChar       ${fg1},${bg2}
        face global WrapMarker         ${fg1},${bg2}
        face global BufferPadding      ${bg0},${bg0}

        # Tree-sitter faces for syntax-based highlights
        face global ts_attribute          ${b_aqua}
        face global ts_comment            ${fg3}+i
        face global ts_constant           ${b_purple}
        face global ts_constant_boolean   ${b_red}
        face global ts_constructor        ${b_blue}+b
        face global ts_diff_plus          ${b_green}
        face global ts_diff_minus         ${b_red}
        face global ts_diff_delta         ${b_blue}
        face global ts_error              ${b_red}+b
        face global ts_function           ${b_green}+b
        face global ts_function_macro     ${b_yellow}+b
        face global ts_hint               ${b_blue}+b
        face global ts_info               ${b_aqua}+b
        face global ts_keyword            ${b_red}
        face global ts_markup_bold        ${fg0}+b
        face global ts_markup_heading     ${b_orange}+b
        face global ts_markup_list_numbered ${b_yellow}
        face global ts_markup_quote       ${fg2}+i
        face global ts_markup_raw         ${fg2}+i
        face global ts_markup_italic      ${fg3}+i
        face global ts_markup_strikethrough ${fg4}+s
        face global ts_namespace          ${b_blue}
        face global ts_operator           ${b_blue}
        face global ts_property           ${b_aqua}
        face global ts_punctuation        ${fg3}
        face global ts_string             ${b_green}
        face global ts_string_escape      ${b_aqua}
        face global ts_tag                ${b_yellow}
        face global ts_type               ${b_yellow}
        face global ts_variable           ${b_blue}
        face global ts_variable_parameter ${fg2}+i
        face global ts_warning            ${b_yellow}+b
    "
}
