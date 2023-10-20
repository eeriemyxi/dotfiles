require("setup_plugin_manager")
require("keymapping")

require("ext.auto_backup")
if vim.g.neovide then
    require("ext.neovide")
end

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
