-- Wrap vim.notify to prevent "E5560: nvim_echo must not be called in a fast event context"
-- errors caused by asynchronous background callbacks (e.g., from project.nvim).
local original_notify = vim.notify
vim.notify = function(msg, level, opts)
  if vim.in_fast_event() then
    vim.schedule(function()
      original_notify(msg, level, opts)
    end)
  else
    original_notify(msg, level, opts)
  end
end

vim.g.mapleader = " "

local opt = vim.opt

opt.virtualedit = "onemore"
opt.completeopt = { "menu", "menuone", "noselect", "popup" }
opt.expandtab = true
opt.shiftwidth = 2
opt.tabstop = 2
opt.softtabstop = 2
opt.number = true
-- opt.relativenumber = true
opt.ignorecase = true
opt.smartcase = true
opt.smartindent = true
opt.splitright = true
opt.splitbelow = true
opt.cursorline = true
opt.wrap = false
opt.scrolloff = 8
opt.sidescrolloff = 8
opt.termguicolors = true
opt.signcolumn = "yes"
opt.updatetime = 250
opt.timeoutlen = 300
opt.clipboard = "unnamedplus"
opt.mouse = "a"
opt.undofile = true
opt.shada = "!,'1000,<50,s10,h"
vim.opt.foldmethod = "expr"
vim.opt.foldexpr = "v:lua.vim.treesitter.foldexpr()"
vim.opt.foldlevel = 99
vim.opt.foldlevelstart = 99

vim.o.autocomplete = true
vim.cmd("filetype plugin indent on")

local function github(repo)
  return { src = "https://github.com/" .. repo }
end

local plugins = {
  github("ellisonleao/gruvbox.nvim"),
  github("nvim-treesitter/nvim-treesitter"),
  github("neovim/nvim-lspconfig"),
  github("jake-stewart/multicursor.nvim"),
  github("echasnovski/mini.files"),
  github("ibhagwan/fzf-lua"),
  github("numToStr/Comment.nvim"),
  github("lewis6991/gitsigns.nvim"),
  github("pocco81/auto-save.nvim"),
  github("folke/which-key.nvim"),
  github("folke/flash.nvim"),
  github("mbbill/undotree"),
  github("karb94/neoscroll.nvim"),
  github("nvim-lua/plenary.nvim"),
  github("DrKJeff16/project.nvim"),
  github("nvim-telescope/telescope.nvim"),
  github("tpope/vim-sleuth"),
  github("sindrets/diffview.nvim"),
  github("NeogitOrg/neogit.git"),
  github("mason-org/mason.nvim"),
  github("mason-org/mason-lspconfig.nvim"),
  github("ray-x/lsp_signature.nvim"),
}

vim.pack.add(plugins)

require("gruvbox").setup({ transparent_mode = true })
vim.cmd.colorscheme("gruvbox")

require("mini.files").setup()
require("Comment").setup()
require("which-key").setup({ delay = 0 })
require("project").setup()
require("auto-save").setup()

require("flash").setup({})
vim.keymap.set({ "n", "x", "o" }, "s", function() require("flash").jump() end, { desc = "Flash" })
vim.keymap.set({ "n", "x", "o" }, "S", function() require("flash").treesitter() end, { desc = "Flash Treesitter" })
vim.keymap.set("o", "r", function() require("flash").remote() end, { desc = "Remote Flash" })
vim.keymap.set({ "o", "x" }, "R", function() require("flash").treesitter_search() end, { desc = "Treesitter Search" })
vim.keymap.set("c", "<c-s>", function() require("flash").toggle() end, { desc = "Toggle Flash Search" })

require("mason").setup {
  firewall = {
    enabled = true
  }
}

require("mason-lspconfig").setup({})

require("neogit").setup({
  remember_settings = true,
  use_per_project_settings = true,
  
  sections = {
    untracked = { folded = false },
    unstaged = { folded = false },
    staged = { folded = false },
    recent = { folded = true },
  },
  
  treesitter_diff_highlight = false,
  word_diff_highlight = true,
  integrations = { diffview = true },
})

require("telescope").setup()
require("telescope").load_extension("projects")
require("neoscroll").setup()
require("lsp_signature").setup()

local gs = require("gitsigns")
gs.setup({
  on_attach = function(bufnr)
    local function map(mode, l, r, opts)
      opts = opts or {}
      opts.buffer = bufnr
      vim.keymap.set(mode, l, r, opts)
    end

    map("n", "]c", function()
      if vim.wo.diff then return "]c" end
      vim.schedule(function() gs.next_hunk() end)
      return "<Ignore>"
    end, { expr = true })

    map("n", "[c", function()
      if vim.wo.diff then return "[c" end
      vim.schedule(function() gs.prev_hunk() end)
      return "<Ignore>"
    end, { expr = true })

    map("n", "<leader>hs", gs.stage_hunk)
    map("n", "<leader>hr", gs.reset_hunk)
    map("n", "<leader>hp", gs.preview_hunk)
    map("n", "<leader>hb", function() gs.blame_line({ full = true }) end)
  end,
})

local mc = require("multicursor-nvim")
mc.setup()

local set = vim.keymap.set

set("n", "]<Space>", "o<Esc>k", { noremap = true, silent = true, desc = "Add line below" })
set("n", "[<Space>", "O<Esc>j", { noremap = true, silent = true, desc = "Add line above" })

set("i", "kj", "<Esc>")
set("i", "<C-Backspace>", "<C-w>", { desc = "Delete word backward" })
set("n", "gp", "`[v`]", { desc = "Select last pasted text" })

local function check_lua_syntax(path)
  local cmd = {
    "nvim",
    "--headless",
    "--clean",
    "-u", "NONE",
    "-c", ("lua assert(loadfile(%q))"):format(path),
    "-c", "qa!",
  }

  local output = vim.fn.system(cmd)
  return vim.v.shell_error == 0, output
end

local function safe_restart()
  vim.cmd("silent! wa")

  local init = vim.fn.stdpath("config") .. "/init.lua"

  local chunk, err = loadfile(init)

  if not chunk then
    vim.notify(
      "Restart aborted:\n" .. err,
      vim.log.levels.ERROR
    )
    return
  end

  vim.cmd("restart")
end

vim.keymap.set("n", "<leader>rs", safe_restart, { desc = "Safe Restart Neovim" })

set("n", "<leader>m", "<cmd>make<CR>")
set("n", "<leader>w", "<cmd>Telescope projects<CR>")
set("n", "<F5>", "<cmd>w | make<CR>")
set("n", "<leader>Co", "<cmd>copen<CR>")
set("n", "<leader>Cc", "<cmd>cclose<CR>")
set("n", "<leader>]", "<cmd>cnext<CR>")
set("n", "<leader>[", "<cmd>cprev<CR>")

set("n", "<leader>H", "<cmd>nohlsearch<CR>")
set("n", "<leader>sv", "<cmd>vsplit<CR>")
set("n", "<leader>sh", "<cmd>split<CR>")
set("n", "<leader>sx", "<cmd>close<CR>")
set("n", "<leader>d", "<C-w>")
set("n", "<leader>q", "<cmd>q<CR>")
set("n", "<leader>F", function()
  if not MiniFiles.close() then MiniFiles.open() end
end)
set("n", "<leader>u", vim.cmd.UndotreeToggle)

set("n", "<leader>g", vim.cmd.Neogit)

local fzf = require("fzf-lua")
set("n", "<leader>ff", fzf.files)
set("n", "<leader>fd", "<cmd>Telescope treesitter<CR>")
set("n", "<leader>fg", fzf.live_grep)
set("n", "<leader>fb", fzf.buffers)
set("n", "<leader>fh", fzf.help_tags)
set("n", "<leader>fr", fzf.oldfiles, { desc = "Find Recent Files" })

set({ "n", "x" }, "<up>", function() mc.lineAddCursor(-1) end)
set({ "n", "x" }, "<down>", function() mc.lineAddCursor(1) end)
set({ "n", "x" }, "<leader><up>", function() mc.lineSkipCursor(-1) end)
set({ "n", "x" }, "<leader><down>", function() mc.lineSkipCursor(1) end)

set({ "n", "x" }, "<leader>n", function() mc.matchAddCursor(1) end)
set({ "n", "x" }, "<leader>s", function() mc.matchSkipCursor(1) end)
set({ "n", "x" }, "<leader>N", function() mc.matchAddCursor(-1) end)
set({ "n", "x" }, "<leader>S", function() mc.matchSkipCursor(-1) end)

set("n", "<c-leftmouse>", mc.handleMouse)
set("n", "<c-leftdrag>", mc.handleMouseDrag)
set("n", "<c-leftrelease>", mc.handleMouseRelease)
set({ "n", "x" }, "<c-q>", mc.toggleCursor)

mc.addKeymapLayer(function(layerSet)
  layerSet({ "n", "x" }, "<left>", mc.prevCursor)
  layerSet({ "n", "x" }, "<right>", mc.nextCursor)
  layerSet({ "n", "x" }, "<leader>x", mc.deleteCursor)
  
  layerSet("n", "<esc>", function()
    if not mc.cursorsEnabled() then
      mc.enableCursors()
    else
      mc.clearCursors()
    end
  end)
end)

local hl = vim.api.nvim_set_hl

hl(0, "CursorLine", { bg = "NONE" }) 
hl(0, "MultiCursorCursor", { reverse = true })
hl(0, "MultiCursorVisual", { link = "Visual" })
hl(0, "MultiCursorSign", { link = "SignColumn" })
hl(0, "MultiCursorMatchPreview", { link = "Search" })
hl(0, "MultiCursorDisabledCursor", { reverse = true })
hl(0, "MultiCursorDisabledVisual", { link = "Visual" })
hl(0, "MultiCursorDisabledSign", { link = "SignColumn" })

-- The actual label character you need to press (e.g., make it bold and bright)
hl(0, "FlashLabel", { fg = "#fe8019", bg = "#3c3836", bold = true })
-- The text matching your search pattern underneath/around the label
hl(0, "FlashMatch", { fg = "#8ec07c", bg = "NONE" })
-- Current target match focus
hl(0, "FlashCurrent", { fg = "#fabd2f", bg = "#504945" })

vim.diagnostic.config({
  virtual_text = { current_line = true },
  signs = true,
  underline = true,
  update_in_insert = false,
})

local function fix_neogit_highlights()
  local hl = vim.api.nvim_set_hl

  hl(0, "NeogitDiffAdd", { fg = "#b8bb26", bg = "NONE" })
  hl(0, "NeogitDiffDelete", { fg = "#fb4934", bg = "NONE" })
  hl(0, "NeogitDiffContext", { bg = "NONE" })

  hl(0, "NeogitDiffAddHighlight", { fg = "#b8bb26", bg = "NONE" })
  hl(0, "NeogitDiffDeleteHighlight", { fg = "#fb4934", bg = "NONE" })
  hl(0, "NeogitDiffContextHighlight", { bg = "NONE" })

  hl(0, "NeogitDiffContextCursor", { bg = "NONE" })
  hl(0, "NeogitDiffAddCursor", { fg = "#b8bb26", bg = "NONE" })
  hl(0, "NeogitDiffDeleteCursor", { fg = "#fb4934", bg = "NONE" })
  hl(0, "NeogitDiffHeaderCursor", { bg = "NONE" })
  hl(0, "NeogitHunkHeaderCursor", { fg = "#fabd2f", bg = "NONE", bold = true })
  
  hl(0, "NeogitCursorLine", { bg = "NONE", underline = true, sp = "#504945" })

  hl(0, "NeogitHunkHeader", { fg = "#fabd2f", bg = "NONE" })
  hl(0, "NeogitHunkHeaderHighlight", { fg = "#fabd2f", bg = "NONE", bold = true })

  hl(0, "NeogitChangeDeleted", { fg = "#fb4934", bg = "NONE", bold = true })

  hl(0, "NeogitDiffAddInline", { bg = "#b8bb26", fg = "#282828", bold = true })
  hl(0, "NeogitDiffDeleteInline", { bg = "#fb4934", fg = "#282828", bold = true })
end

fix_neogit_highlights()

vim.api.nvim_create_autocmd("ColorScheme", {
  pattern = "*",
  callback = fix_neogit_highlights,
})

local view_group = vim.api.nvim_create_augroup("AutoView", { clear = true })

vim.api.nvim_create_autocmd({ "BufWinLeave" }, {
  group = view_group,
  pattern = { "*.*" },
  desc = "Save view when closing file",
  callback = function()
    if vim.bo.buftype == "" and vim.fn.expand("%") ~= "" then
      vim.cmd("mkview")
    end
  end,
})

vim.api.nvim_create_autocmd({ "BufWinEnter" }, {
  group = view_group,
  pattern = { "*.*" },
  nested = true,
  desc = "Load view when opening file",
  callback = function()
    if vim.bo.buftype == "" and vim.fn.expand("%") ~= "" then
      vim.cmd("silent! loadview")
    end
  end,
})

local last_cmd = ""
local active_job_id = nil
local stream_buf = ""

local group = vim.api.nvim_create_augroup("CompileQF", { clear = true })
vim.api.nvim_create_autocmd("FileType", {
  group = group,
  pattern = "qf",
  callback = function(args)
    vim.keymap.set("n", "q", "<cmd>cclose<cr>", { buffer = args.buf, nowait = true, silent = true })
  end,
})

vim.api.nvim_create_user_command("Compile", function()
  vim.ui.input({ prompt = "Compile command: ", default = last_cmd }, function(input)
    if not input or input == "" then return end
    last_cmd = input

    if active_job_id then
      vim.fn.jobstop(active_job_id)
    end

    stream_buf = ""
    vim.opt.errorformat = "%f(%l:%c) %t%*[^:]: %m"
    vim.fn.setqflist({}, "r", { title = input, items = {} })

    local current_win = vim.api.nvim_get_current_win()
    vim.cmd("botright copen 10")
    if vim.api.nvim_win_is_valid(current_win) then
      vim.api.nvim_set_current_win(current_win)
    end

    local function append_to_qf(_, data)
      if not data then return end
      
      data[1] = stream_buf .. data[1]
      stream_buf = table.remove(data) or ""

      if #data > 0 then
        vim.fn.setqflist({}, "a", { lines = data })
        vim.cmd("cbottom")
      end
    end

    active_job_id = vim.fn.jobstart(vim.fn.expand(input), {
      stdout_buffered = false,
      stderr_buffered = false,
      on_stdout = append_to_qf,
      on_stderr = append_to_qf,
      on_exit = function(_, code)
        active_job_id = nil
        if stream_buf ~= "" then
          vim.fn.setqflist({}, "a", { lines = { stream_buf } })
          stream_buf = ""
        end
        vim.fn.setqflist({}, "a", { title = string.format("%s (Exit code: %d)", input, code) })
        vim.cmd("cbottom")
      end,
    })
  end)
end, {})

vim.keymap.set("n", "<leader>c", "<cmd>Compile<cr>")
