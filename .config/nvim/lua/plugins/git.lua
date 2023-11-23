-- return {
--   "NeogitOrg/neogit",
--   dependencies = {
--     "nvim-lua/plenary.nvim",         -- required
--     "nvim-telescope/telescope.nvim", -- optional
--     "sindrets/diffview.nvim",        -- optional
--     "ibhagwan/fzf-lua",              -- optional
--   },
--   config = true
-- }

return {
        "kdheepak/lazygit.nvim",
        -- optional for floating window border decoration
        dependencies = {
            "nvim-lua/plenary.nvim",
        },
    }
