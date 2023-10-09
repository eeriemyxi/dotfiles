return {
    "nvim-tree/nvim-tree.lua",
    version = "*",
    lazy = false,
    dependencies = {
        "nvim-tree/nvim-web-devicons",
    },
    config = function()
        require("nvim-tree").setup({
            on_attach = function(bufnr)
                api = require("nvim-tree.api")

                function opts(desc)
                    return {
                        desc = string.format("nvim-tree: %s", desc),
                        buffer = bufnr,
                        noremap = true,
                        silent = true,
                        nowait = true,
                    }
                end

                api.config.mappings.default_on_attach(bufnr)

                vim.keymap.set("n", "h", api.fs.rename_basename, opts("Rename"))
                vim.keymap.set("n", "e", "k", opts("k"))
            end,
            actions = { expand_all = { exclude = { ".backup", ".git" } } },
        })
        vim.cmd.autocmd([[VimEnter * NvimTreeOpen]])
        vim.cmd.autocmd([[VimEnter * call feedkeys("\<C-W>W")]])
    end,
}
