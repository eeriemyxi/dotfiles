vim.keymap.set("n", "<Space>f", "<cmd>Telescope find_files<cr>")

vim.api.nvim_set_keymap('n', 'n', 'j', {noremap = true})
vim.api.nvim_set_keymap('n', 'e', 'k', {noremap = true})
vim.api.nvim_set_keymap('n', 'i', 'l', {noremap = true})

vim.api.nvim_set_keymap('n', 'u', 'i', {noremap = true})

vim.api.nvim_set_keymap('n', '<Alt>c', '<Esc>', {noremap = true})
vim.api.nvim_set_keymap('i', '<Alt>c', '<Esc>', {noremap = true})

vim.keymap.del("n", "<Esc>")
