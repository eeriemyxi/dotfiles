require("lazy_setup")
require("keymapping")
require("lualine").setup({ options = { theme = "gruvbox" } })

vim.opt.smarttab = true
vim.opt.undofile = true
vim.opt.termguicolors = true

vim.opt.backup = true
vim.opt.backupdir = "./.backup"

vim.opt.virtualedit = "onemore"
