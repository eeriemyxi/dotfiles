require("lazy_setup")
require("keymapping")

require("ext.auto_backup")

require("lualine").setup({ options = { theme = "gruvbox" } })

vim.opt.smarttab = true
vim.opt.undofile = true
vim.opt.termguicolors = true

vim.opt.virtualedit = "onemore"
vim.opt.clipboard = "unnamedplus"
