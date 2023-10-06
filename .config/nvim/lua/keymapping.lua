vim.keymap.set("n", "<Space>f", "<cmd>Telescope find_files<cr>")
vim.keymap.set("i", "hh", "<cmd>stopi<cr>")
vim.keymap.set("n", "x", "0v$")

vim.keymap.set("n", "] ", "o<Esc>")
vim.keymap.set("n", "[ ", "ko<Esc>")

vim.keymap.set({"n", "i"}, "<Space>T", "<cmd>CHADopen<cr>")
vim.keymap.set({"n", "i"}, "<Space>w", "<C-W>W")

vim.keymap.set({"n", "v"}, "n", "j")
vim.keymap.set({"n", "v"}, "e", "k")
vim.keymap.set({"n", "v"}, "i", "l")
vim.keymap.set({"n", "v"}, "l", "u")

vim.keymap.set({"n", "v"}, "u", "i")

vim.keymap.set({"n", "v"}, "gl", "$")
vim.keymap.set({"n", "v"}, "gh", "0")

vim.api.nvim_set_var("chadtree_settings", {["view.width"]=30})
vim.cmd.autocmd([[VimEnter * CHADopen)]])
vim.cmd.autocmd([[VimEnter * call feedkeys("\<C-W>W")]])
