return {
    "neovim/nvim-lspconfig",
    config = function()
        local cmp_nvim_lsp = require("cmp_nvim_lsp")
        local capabilities = cmp_nvim_lsp.default_capabilities()
        local lspconfig = require("lspconfig")
        local pylsp_config = {
            settings = { pylsp = { plugins = { flake8 = { enabled = true, maxLineLength = 88 } } } },
        }

        lspconfig.pylsp.setup({ capabilities = capabilities })
    end,
}
