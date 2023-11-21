return {
    { "hrsh7th/cmp-buffer", event = "VeryLazy" },
    { "hrsh7th/cmp-cmdline", event = "VeryLazy" },
    { "hrsh7th/cmp-nvim-lsp", event = "LspAttach" },
    { "hrsh7th/cmp-path", event = "VeryLazy" },
    {
        "hrsh7th/nvim-cmp",
        event = "VeryLazy",
        config = function()
            local cmp = require("cmp")

            cmp.setup({
                snippet = {
                    expand = function(args)
                        require("snippy").expand_snippet(
                            args.body
                        )
                    end,
                },

                window = {
                    completion = cmp.config.window.bordered(),
                    documentation = cmp.config.window.bordered(),
                },

                mapping = cmp.mapping.preset.insert({

                    ["<C-n>"] = cmp.mapping(
                        function(fallback)
                            if cmp.visible() then
                                cmp.select_next_item()
                            elseif
                                snippy.can_expand_or_advance()
                            then
                                snippy.expand_or_advance()
                            elseif has_words_before() then
                                cmp.complete()
                            else
                                fallback()
                            end
                        end,
                        { "i", "s" }
                    ),

                    ["<C-e>"] = cmp.mapping(
                        function(fallback)
                            if cmp.visible() then
                                cmp.select_prev_item()
                            elseif snippy.can_jump(-1) then
                                snippy.previous()
                            else
                                fallback()
                            end
                        end,
                        { "i", "s" }
                    ),

                    ["<C-u>"] = cmp.mapping.scroll_docs(-4),
                    ["<C-l>"] = cmp.mapping.scroll_docs(4),
                    ["<C-Space>"] = cmp.mapping.complete(),
                    ["<C-y>"] = cmp.mapping.abort(),
                    ["<CR>"] = cmp.mapping.confirm({
                        select = true,
                    }),
                }),

                sources = cmp.config.sources({
                    { name = "nvim_lsp" },
                    { name = "snippy" },
                }, {
                    { name = "buffer" },
                    { name = "codeium" },
                }),
            })

            cmp.setup.filetype("gitcommit", {
                sources = cmp.config.sources({
                    { name = "git" },
                }, {
                    { name = "buffer" },
                }),
            })

            cmp.setup.cmdline({ "/", "?" }, {
                mapping = cmp.mapping.preset.cmdline(),
                sources = {
                    { name = "buffer" },
                },
            })

            cmp.setup.cmdline(":", {
                mapping = cmp.mapping.preset.cmdline(),
                sources = cmp.config.sources({
                    { name = "path" },
                }, {
                    { name = "cmdline" },
                }),
            })
        end,
    },
}
