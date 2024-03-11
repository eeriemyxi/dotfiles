return {
    "nvim-lualine/lualine.nvim",
    event = "VeryLazy",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    cond = not vim.g.started_by_firenvim,
    config = function()
        require("lualine").setup({
            options = { theme = "gruvbox" },
        })
    end,
}
