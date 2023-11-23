require("setup_plugin_manager")
require("keymapping")

require("ext.auto_backup")

if vim.g.neovide then
    require("ext.neovide")
end

vim.g.firenvim_config = {
    globalSettings  =  {  alt  =  "all"  },
    localSettings   =  {                 
        [".*"]          = {
            cmdline   =  "neovim",
            content   =  "text",
            priority  =  0,
            selector  =  "textarea",
            takeover  =  "always",
        },
        ["discord.com"] = {
            takeover  = "never",
        },
    },
}

if vim.g.started_by_firenvim then
    vim.cmd([[
        set noshowmode
        set noruler
        set laststatus=0
        set noshowcmd
    ]])
else
    -- Load Discord RPC
    local rpc_load_command =
        "node /home/$USER/Documents/arRPC/src"
    local check_process_command = "ps x | grep '"
        .. rpc_load_command
        .. "'| grep -v grep"
    vim.fn.jobstart(
        string.format(
            "if [[ -z $(%s) ]]; then %s; fi",
            check_process_command,
            rpc_load_command
        )
    )
    require("presence")

    -- Aliases
    vim.cmd([[Alias -range align !column\ -t]])
    vim.cmd([[Alias seediff w\ !diff\ %\ -]])
end

vim.opt.smarttab       =  true
vim.opt.undofile       =  true
vim.opt.termguicolors  =  true
vim.opt.autochdir      =  false
vim.opt.shiftround     =  true
vim.opt.expandtab      =  true
vim.opt.autoindent     =  true
vim.opt.smartindent    =  true

vim.opt.tabstop        =  4
vim.opt.softtabstop    =  -1
vim.opt.shiftwidth     =  0
vim.opt.timeoutlen     =  250

vim.opt.virtualedit    =  "onemore"
vim.opt.clipboard      =  "unnamedplus"

vim.cmd [[
set iskeyword-=_
]]
