vim.keymap.set("n", "<Space>f", "<cmd>Telescope find_files<cr>")
vim.keymap.set("i", "hh", "<cmd>stopi<cr>")

local map = vim.api.nvim_set_keymap

map("n", "n", "j", { noremap = true })
map("n", "e", "k", { noremap = true })
map("n", "i", "l", { noremap = true })
map("n", "l", "u", { noremap = true })
map("n", "u", "i", { noremap = true })

map("n", "gl", "$", { noremap = true })
map("n", "gh", "0", { noremap = true })
