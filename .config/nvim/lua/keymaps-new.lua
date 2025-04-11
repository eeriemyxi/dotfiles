-- WIP

vim.cmd [[ 
inoremap kt <Esc> col('.') == 1 ? "<Esc>" : "<Esc>l"
inoremap <Esc> <Esc> col('.') == 1 ? "<Esc>" : "<Esc>l"
vnoremap <space> <Esc>
]]

vim.keymap.set("n", "v", "i")
vim.keymap.set("n", "<space>", "v")

vim.keymap.set({ "n", "v" }, "N", "<C-D>")
vim.keymap.set({ "n", "v" }, "E", "<C-U>")

vim.keymap.set({ "n", "v" }, "n", "j")
vim.keymap.set({ "n", "v" }, "e", "k")
vim.keymap.set({ "n", "v" }, "i", "l")
vim.keymap.set({ "n", "v" }, "l", "u")

vim.keymap.set({ "n", "v" }, "N", "}")
vim.keymap.set({ "n", "v" }, "E", "{")
vim.keymap.set({ "n", "v" }, "H", "b")
vim.keymap.set({ "n", "v" }, "I", "w")
