vim.keymap.set(
    "n",
    "<Space>f",
    "<cmd>Telescope find_files<cr>"
)
vim.keymap.set(
    "n",
    "<Space>F",
    "<cmd>Telescope buffers<cr>"
)
vim.keymap.set(
    "n",
    "<Space>d",
    "<cmd>Telescope diagnostics<cr>"
)
vim.keymap.set("i", "hh", "<cmd>stopi<cr>")
vim.keymap.set("n", "x", "\"_x")
vim.keymap.set("v", "x", "\"_d")

vim.keymap.set("n", "] ", "o<Esc>")
vim.keymap.set("n", "L", "<cmd>redo<cr>")
vim.keymap.set("n", "U", "~")
vim.keymap.set("n", "[ ", "O<Esc>")

vim.keymap.set(
    { "n" },
    "<Space>tr",
    "<cmd>NvimTreeToggle<cr>"
)
vim.keymap.set({ "n" }, "<Space>w", "<C-W>W")

vim.keymap.set({ "n", "v" }, "k", "n")

vim.keymap.set({ "n", "v" }, "N", "<C-D>")
vim.keymap.set({ "n", "v" }, "E", "<C-U>")
vim.keymap.set({ "n", "v" }, "k", "n")
vim.keymap.set({ "n" }, "d", "\"_d")
vim.keymap.set({ "n", "v" }, "<Space>h", "<C-W>h")
vim.keymap.set({ "n", "v" }, "<Space>n", "<C-W>j")
vim.keymap.set({ "n", "v" }, "<Space>e", "<C-W>k")
vim.keymap.set({ "n", "v" }, "<Space>i", "<C-W>l")

vim.keymap.set({ "n", "v" }, "<Space>H", "<C-W>H")
vim.keymap.set({ "n", "v" }, "<Space>N", "<C-W>J")
vim.keymap.set({ "n", "v" }, "<Space>E", "<C-W>K")
vim.keymap.set({ "n", "v" }, "<Space>I", "<C-W>L")

vim.keymap.set({ "n", "v" }, "n", "j")
vim.keymap.set({ "n", "v" }, "e", "k")
vim.keymap.set({ "n", "v" }, "i", "l")
vim.keymap.set({ "n", "v" }, "l", "u")

vim.keymap.set({ "n", "v" }, "u", "i")

vim.keymap.set({ "n", "v" }, "gl", "$")
vim.keymap.set({ "n", "v" }, "gh", "0")

vim.keymap.set({"i", "n"}, "<A-BS>", "<Esc>vb\"_di")
