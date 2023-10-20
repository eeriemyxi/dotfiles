return {
    "neovim/nvim-lspconfig",
    event = "VeryLazy",
    config = function()
        local cmp_nvim_lsp = require("cmp_nvim_lsp")
        local capabilities =
            cmp_nvim_lsp.default_capabilities()

        local lspconfig = require("lspconfig")
        local pylsp_config = {
            pylsp = {
                plugins = {
                    ruff = { enable = true },
                    pylsp_mypy = { enabled = false },
                },
            },
        }
        local util = require("lspconfig/util")

        lspconfig.pyright.setup({
            root_dir = function(fname)
                return util.root_pattern(
                    ".git",
                    "setup.py",
                    "setup.cfg",
                    "pyproject.toml",
                    "requirements.txt"
                )(fname) or util.path.dirname(
                    fname
                )
            end,
            capabilities = capabilities,
        })
        lspconfig.pylsp.setup({
            settings = pylsp_config,
            capabilities = capabilities,
        })
        -- lspconfig.pyright.setup({capabilities = capabilities})
    end,
}
