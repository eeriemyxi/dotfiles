vim.loader.enable()

-- Neovide settings
if vim.g.neovide then
  vim.g.neovide_opacity = 0.9
  vim.g.neovide_normal_opacity = 0.9
  vim.o.winblend = 20
  vim.o.pumblend = 20
  vim.g.neovide_cursor_animate_in_insert_mode = true
  vim.g.neovide_floating_blur_amount_x = 2.0
  vim.g.neovide_floating_blur_amount_y = 2.0
  vim.g.neovide_floating_corner_radius = 0.3
  vim.g.neovide_floating_z_height = 10.0
  vim.g.neovide_scale_factor = 1.0
  vim.g.neovide_text_gamma = 1
  vim.g.neovide_text_contrast = 0.1
  vim.g.neovide_refresh_rate = 120
  vim.o.guifont = "Maple Mono NF,JetBrainsMono_Nerd_Font:h9:#e-subpixelantialias:#h-slight"
  vim.g.neovide_padding_left = 12
  vim.g.neovide_padding_right = 12
  vim.g.neovide_padding_top = 12
  vim.g.neovide_padding_bottom = 12
  vim.g.neovide_cursor_vfx_mode = "pixiedust"
  vim.g.neovide_cursor_vfx_opacity = 200
  vim.g.neovide_cursor_vfx_particle_lifetime = 1.5
  vim.g.neovide_cursor_vfx_particle_highlight_lifetime = 0.2
  vim.g.neovide_cursor_vfx_particle_density = 1.5
  vim.g.neovide_cursor_short_animation_length = 0.54

  vim.keymap.set({ "n", "v" }, "<C-=>", function()
    vim.g.neovide_scale_factor = vim.g.neovide_scale_factor + 0.1
  end, { desc = "Increase Neovide Scale" })

  vim.keymap.set({ "n", "v" }, "<C-->", function()
    vim.g.neovide_scale_factor = vim.g.neovide_scale_factor - 0.1
  end, { desc = "Decrease Neovide Scale" })

  vim.keymap.set({ "n", "v" }, "<C-0>", function()
    vim.g.neovide_scale_factor = 1.0
  end, { desc = "Reset Neovide Scale" })
end

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
-- opt.completeopt = { "menu", "menuone", "noselect", "popup" }
-- vim.o.autocomplete = true
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
vim.opt.directory = vim.fn.stdpath("cache") .. "/swap//"
vim.opt.shortmess:append("A")
vim.opt.viewoptions:remove("curdir")
vim.opt.iskeyword:remove("_")

vim.cmd("filetype plugin indent on")

local function gh(repo)
  return { src = "https://github.com/" .. repo }
end

local plugins = {
  gh "sainnhe/gruvbox-material",
  gh "nvim-treesitter/nvim-treesitter",
  gh "neovim/nvim-lspconfig",
  gh "jake-stewart/multicursor.nvim",
  gh "echasnovski/mini.files",
  gh "ibhagwan/fzf-lua",
  gh "lewis6991/gitsigns.nvim",
  gh "pocco81/auto-save.nvim",
  gh "folke/which-key.nvim",
  gh "folke/flash.nvim",
  gh "ej-shafran/compile-mode.nvim",
  gh "mbbill/undotree",
  gh "karb94/neoscroll.nvim",
  gh "nvim-lua/plenary.nvim",
  gh "DrKJeff16/project.nvim",
  gh "tpope/vim-sleuth",
  gh "sindrets/diffview.nvim",
  gh "NeogitOrg/neogit.git",
  gh "mason-org/mason.nvim",
  gh "mason-org/mason-lspconfig.nvim",
  gh "rafamadriz/friendly-snippets",
  gh "saghen/blink.lib",
  gh "saghen/blink.cmp",
  gh "nvim-lualine/lualine.nvim",
}

vim.pack.add(plugins, { shallow = true })

require("lualine").setup({
  options = {
    theme = 'gruvbox-material'
  }
})

vim.g.gruvbox_material_enable_italic = true
-- vim.g.gruvbox_material_enable_bold = true
vim.g.gruvbox_material_transparent_background = not vim.g.neovide
vim.g.gruvbox_material_foreground = "original"
vim.g.gruvbox_material_background = "medium"
vim.cmd.colorscheme('gruvbox-material')

local cmp = require('blink.cmp')
cmp.build():pwait()
cmp.setup({
  completion = { documentation = { auto_show = true } },
  signature = { enabled = true },
  cmdline = {
    enabled = true,
    keymap = { preset = "cmdline" },
    completion = { menu = { auto_show = true } },
  },
})

require("mini.files").setup()
require("which-key").setup({ delay = 0 })
require("project").setup({
  manual_mode = true,
  fzf_lua = {
    enabled = true
  }
})

-- automatically save project if found without editing cwd
vim.api.nvim_create_autocmd({ "BufEnter", "VimEnter" }, {
  pattern = "*",
  callback = function()
    local core = require("project.core")
    local history = require("project.util.history")
    local root = core.get_project_root()

    if root then
      local exists = vim.tbl_contains(history.session_projects, function(val)
        return val.path == root
      end, { predicate = true })

      if not exists then
        table.insert(history.session_projects, 1, {
          path = root,
          name = vim.fs.basename(root)
        })
        history.write_history()
      end
    end
  end
})
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
    untracked = { folded = true },
    unstaged = { folded = false },
    staged = { folded = false },
    recent = { folded = true },
  },

  treesitter_diff_highlight = false,
  word_diff_highlight = true,
  integrations = { diffview = true },
})

if not vim.g.neovide then
  require("neoscroll").setup({
    duration_multiplier = 1.0,
  })
end

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

local paste_ns = vim.api.nvim_create_namespace("robust_paste")

local function robust_paste(cmd)
  return function()
    -- Capture user-provided counts and registers before execution
    local count = vim.v.count > 0 and vim.v.count or ""
    local reg = vim.v.register == '"' and "" or '"' .. vim.v.register

    vim.api.nvim_buf_clear_namespace(0, paste_ns, 0, -1)

    -- Execute paste synchronously
    vim.cmd('normal! ' .. reg .. count .. cmd)

    -- Capture marks immediately before async tools can modify them
    local start_pos = vim.api.nvim_buf_get_mark(0, "[")
    local end_pos = vim.api.nvim_buf_get_mark(0, "]")

    if start_pos[1] == 0 or end_pos[1] == 0 then return end

    -- Bind extmark to the start of the paste
    vim.b.paste_start_id = vim.api.nvim_buf_set_extmark(0, paste_ns, start_pos[1] - 1, start_pos[2], {})

    -- Calculate bounds to prevent column overflow crashes
    local line_len = #vim.api.nvim_buf_get_lines(0, end_pos[1] - 1, end_pos[1], false)[1]
    local end_col = math.min(end_pos[2], math.max(0, line_len - 1))

    -- Bind extmark to the end of the paste
    vim.b.paste_end_id = vim.api.nvim_buf_set_extmark(0, paste_ns, end_pos[1] - 1, end_col, {})
    vim.b.paste_regtype = vim.fn.getregtype(vim.v.register)
  end
end

vim.keymap.set("n", "p", robust_paste("p"), { desc = "Paste and track with extmarks" })
vim.keymap.set("n", "P", robust_paste("P"), { desc = "Paste and track with extmarks" })

vim.keymap.set("n", "gp", function()
  local start_id = vim.b.paste_start_id
  local end_id = vim.b.paste_end_id
  if not start_id or not end_id then return end

  -- Retrieve current extmark positions (shifted automatically by Neovim)
  local start_pos = vim.api.nvim_buf_get_extmark_by_id(0, paste_ns, start_id, {})
  local end_pos = vim.api.nvim_buf_get_extmark_by_id(0, paste_ns, end_id, {})

  if #start_pos == 0 or #end_pos == 0 then return end

  local reg_type = vim.b.paste_regtype or "v"
  local v_mode = "v"
  if reg_type == "V" then
    v_mode = "V"
  elseif reg_type:sub(1, 1) == "\22" or reg_type == "b" then
    v_mode = "\22"
  end

  -- Target positions safely
  pcall(vim.api.nvim_win_set_cursor, 0, {start_pos[1] + 1, start_pos[2]})
  vim.cmd("normal! " .. v_mode)

  -- Clamp end column dynamically based on current buffer state
  local line_len = #vim.api.nvim_buf_get_lines(0, end_pos[1], end_pos[1] + 1, false)[1]
  local end_col = math.min(end_pos[2], math.max(0, line_len - 1))

  pcall(vim.api.nvim_win_set_cursor, 0, {end_pos[1] + 1, end_col})
end, { desc = "Select robustly via extmarks" })

set({'n', 'v'}, 'H', '^')
set({'n', 'v'}, 'L', '$')

local function safe_restart()
  -- vim.cmd("silent! wa")

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

set("n", "<leader>H", "<cmd>nohlsearch<CR>")
set("n", "<leader>sv", "<cmd>vsplit<CR>")
set("n", "<leader>sh", "<cmd>split<CR>")
set("n", "<leader>sx", "<cmd>close<CR>")
set("n", "<leader>d", "<C-w>")
set("n", "<leader>q", "<cmd>q<CR>")
set("n", "<leader>F", function()
  if not MiniFiles.close() then
    MiniFiles.open(vim.api.nvim_buf_get_name(0))
  end
end)
set("n", "<leader>u", vim.cmd.UndotreeToggle)

set("n", "<leader>jf", function()
  local project_lib = require("project")
  local root = project_lib.get_project_root() or vim.fn.getcwd()

  if not MiniFiles.close() then
    MiniFiles.open(root)
  end
end, { desc = "Open mini.files at project root" })

vim.g.compile_mode = {
  error_regexp_table = {
    odin = {
      regex = [[\v^([a-zA-Z0-9_./\-]+)\((\d+):(\d+)\)\s+([^:]+):\s+(.+)$]],
      filename = 1,
      line = 2,
      col = 3,
      type = 4,
    },
  },
}
set("n", "<leader>]", "<cmd>NextError<CR>")
set("n", "<leader>[", "<cmd>PrevError<CR>")

set("n", "<leader>g", function()
  local project_lib = require("project")
  local root = project_lib.get_project_root() or vim.fn.getcwd()
  require("neogit").open({ cwd = root })
end, { desc = "Open Neogit at Project Root" })

local fzf = require("fzf-lua")
fzf.setup({
  hls = {
    cursorline = "Visual"
  },
  winopts = {
    border = "solid",
    preview = {
      border = "solid"
    }
  },
})

local project_lib = require("project")
set("n", "<leader>ff", function()
  local root = project_lib.get_project_root() or vim.fn.getcwd()
  fzf.files({ cwd = root })
end, { desc = "Find Files in Project" })
set("n", "<leader>fg", function()
  local root = project_lib.get_project_root() or vim.fn.getcwd()
  fzf.live_grep({ cwd = root })
end, { desc = "Live Grep in Project" })
set("n", "<leader>fd", fzf.treesitter)
set("n", "<leader>fj", fzf.blines)
set("n", "<leader>fb", fzf.buffers)
set("n", "<leader>fh", fzf.help_tags)
set("n", "<leader>fp", "<cmd>Project fzf-lua<CR>")
local bp = vim.fn.stdpath("data") .. "/oldfiles_blacklist.txt"
local function fr()
  local bl, f = {}, io.open(bp, "r")
  if f then for l in f:lines() do bl[l] = true end f:close() end

  vim.v.oldfiles = vim.tbl_filter(function(v) return not bl[v] end, vim.v.oldfiles)

  require("fzf-lua").oldfiles({
    desc = "Find Recent Files",
    actions = {
      ["alt-d"] = function(sel, opts)
        if not sel[1] then return end
        local p = require("fzf-lua").path.entry_to_file(sel[1], opts).path

        local af = io.open(bp, "a")
        if af then af:write(p .. "\n") af:close() end

        vim.v.oldfiles = vim.tbl_filter(function(v) return v ~= p end, vim.v.oldfiles)
        vim.schedule(fr)
      end
    }
  })
end
vim.keymap.set("n", "<leader>fr", fr, { desc = "Find Recent Files" })

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

local function make_keywords_bold()
  for name, _ in pairs(vim.api.nvim_get_hl(0, {})) do
    if name:lower():find("keyword") then
      local hl_def = vim.api.nvim_get_hl(0, { name = name, link = false })
      hl_def.bold = true
      vim.api.nvim_set_hl(0, name, hl_def)
    end
  end
end
vim.api.nvim_create_autocmd("ColorScheme", {
  pattern = "*",
  callback = make_keywords_bold,
})
make_keywords_bold()

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

local project_compile_cmds = {}

local function smart_project_compile()
  local project_lib = require("project")
  local root = project_lib.get_project_root()
  root = root or vim.fn.getcwd()
  local default_cmd = project_compile_cmds[root] or "make"

  vim.ui.input({
    prompt = "Compile project (" .. vim.fn.fnamemodify(root, ":t") .. "): ",
    default = default_cmd
  }, function(input)
    if not input or input == "" then return end

    project_compile_cmds[root] = input

    local prev_cwd = vim.fn.getcwd()
    vim.api.nvim_set_current_dir(root)

    vim.cmd("Compile " .. input)

    vim.api.nvim_set_current_dir(prev_cwd)
  end)
end

local function smart_project_recompile()
  local project_lib = require("project")
  local root = project_lib.get_project_root() or vim.fn.getcwd()

  local cmd = project_compile_cmds[root]
  if not cmd then
    return smart_project_compile()
  end

  local prev_cwd = vim.fn.getcwd()
  vim.api.nvim_set_current_dir(root)
  vim.cmd("Compile " .. cmd)
  vim.api.nvim_set_current_dir(prev_cwd)
end

vim.keymap.set("n", "<leader>C", smart_project_compile, { desc = "Projectile-like Compile" })
vim.keymap.set("n", "<leader>c", smart_project_recompile, { desc = "Projectile-like Recompile" })
vim.keymap.set("n", "<leader>P", "<cmd>Compile<CR>", { desc = "Compile" })
vim.keymap.set("n", "<leader>p", "<cmd>Recompile<CR>", { desc = "Recompile" })

-- Load up last exit position for a file when opened
vim.api.nvim_create_autocmd("BufReadPost", {
  callback = function()
    local mark = vim.api.nvim_buf_get_mark(0, '"')
    local lcount = vim.api.nvim_buf_line_count(0)
    if mark[1] > 0 and mark[1] <= lcount then
      pcall(vim.api.nvim_win_set_cursor, 0, mark)
    end
  end,
})
