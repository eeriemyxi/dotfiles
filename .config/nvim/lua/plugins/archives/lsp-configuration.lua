return {
    "neovim/nvim-lspconfig",
    event = "VeryLazy",
    config = function()
        local cmp_nvim_lsp = require("cmp_nvim_lsp")
        local capabilities =
            cmp_nvim_lsp.default_capabilities()

        local lspconfig = require("lspconfig")
        -- local pylsp_config = {
        --     pylsp = {
        --         plugins = {
        --             ruff = { enable = true },
        --         },
        --     },
        -- }
        local util = require("lspconfig/util")

        lspconfig.pyright.setup({
            capabilities = capabilities,
        })
        lspconfig.nimls.setup({
            capabilities = capabilities,
        })
        -- lspconfig.pylsp.setup({
        --     settings = pylsp_config,
        --     capabilities = capabilities,
        -- })
        lspconfig.ruff_lsp.setup({
            init_options = {
                settings = {
                    args = {},
                },
            },
            capabilities = capabilities,
        })
        lspconfig.clangd.setup({
            capabilities = capabilities,
        })
    end,
}
