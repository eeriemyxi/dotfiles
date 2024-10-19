return {
    {
        "nvim-telescope/telescope-fzf-native.nvim",
        lazy = true,
        build = "cmake -S. -Bbuild -DCMAKE_BUILD_TYPE=Release && cmake --build build --config Release && cmake --install build --prefix build",
    },
    {
        "nvim-telescope/telescope.nvim",
        event = "VeryLazy",
        dependencies = { "nvim-lua/plenary.nvim" },
        cond = not vim.g.started_by_firenvim,
        config = function()
            tel = require("telescope")
            tel.setup({
                defaults = {
                    sorting_strategy = "ascending",
                    layout_strategy = "horizontal",
                    layout_config = {
                        horizontal = {
                            prompt_position = "top",
                            preview_width = 0.55,
                            results_width = 0.8,
                        },
                        vertical = {
                            mirror = false,
                        },
                        width = 0.87,
                        height = 0.80,
                        preview_cutoff = 120,
                    },
                    initial_mode = "insert",
                    },
            pickers = {
                find_files = {
                    hidden = true,
                }
                },
            })
            tel.load_extension("fzf")
            tel.load_extension("ag")
        end,
    },
}
