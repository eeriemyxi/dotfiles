return {
    "nvim-treesitter/nvim-treesitter",
    lazy = false,
    config = function()
        require("nvim-treesitter.configs").setup({
            ensure_installed = {
                "c",
                "javascript",
                "lua",
                "markdown",
                "markdown_inline",
                "python",
                "query",
                "vim",
                "vimdoc",
            },
            sync_install = true,
            -- auto_install = true,
            highlight = {
                enable = true,
                disable = { "nim" },
                -- disable = function(lang, buf)
                --     local max_filesize = 100 * 1024
                --     local ok, stats = pcall(
                --         vim.loop.fs_stat,
                --         vim.api.nvim_buf_get_name(buf)
                --     )
                --     if
                --         ok
                --         and stats
                --         and stats.size > max_filesize
                --     then
                --         return true
                --     end
                -- end,
                additional_vim_regex_highlighting = true,
            },
        })
    end,
}
