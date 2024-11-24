require("plugin-manager")
require("keymaps")

-- use undo branches instead
-- require("ext.auto-backup")

if vim.g.neovide then
    require("ext.neovide")
end

-- require("ext.latex")

vim.g.mkdp_echo_preview_url = 1

-- Aliases
vim.cmd([[Alias -range align !column\ -t]])
vim.cmd([[Alias seediff w\ !diff\ %\ -]])
vim.cmd("Alias MaT MarkdownPreviewToggle")

vim.opt.smarttab = true
vim.opt.undofile = true
vim.opt.termguicolors = true
vim.opt.autochdir = false
vim.opt.shiftround = true
vim.opt.expandtab = true
vim.opt.autoindent = true
vim.opt.smartindent = true

vim.opt.tabstop = 4
vim.opt.softtabstop = -1
vim.opt.shiftwidth = 0
vim.opt.timeoutlen = 250

vim.opt.virtualedit = "onemore"
vim.opt.clipboard = "unnamedplus"
vim.opt.signcolumn = "no"
-- vim.opt.wrap = true
-- vim.opt.textwidth = 88
-- vim.opt.linebreak = true
-- vim.opt.breakat = "88"
-- vim.opt.colorcolumn = "88"

vim.cmd([[
    autocmd FileType text,markdown setlocal wrap textwidth=88 linebreak
]])

vim.cmd([[
    set iskeyword-=_
    set nofoldenable
    runtime! ftplugin/man.vim
]])
