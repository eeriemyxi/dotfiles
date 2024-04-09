return {
    -- color scheme
    {
        "morhetz/gruvbox",
        priority = 1000,
        config = function()
            vim.cmd.colorscheme("gruvbox")
            vim.cmd("hi Normal guibg=#282828")
        end,
    },
    -- colorized delimiters
    {
        "hiphish/rainbow-delimiters.nvim",
        init = function()
            vim.g.rainbow_delimiters = {
                highlight = {
                    "GruvboxOrange",
                    "GruvboxYellow",
                    "GruvboxBlue",
                    "GruvboxGreen",
                    "GruvboxViolet",
                    "GruvboxCyan",
                    "GruvboxRed",
                },
            }
        end,
    },
    -- colorized comments
    {
        "folke/todo-comments.nvim",
        dependencies = { "nvim-lua/plenary.nvim" },
        opts = {
            keywords = {
                TODO = {
                    icon = " ",
                    color = "Green",
                },
                HACK = {
                    icon = " ",
                    color = "Red",
                },
                WARN = {
                    icon = " ",
                    color = "Red",
                    alt = { "WARNING", "XXX", "FIXME" },
                },
                PERF = {
                    icon = " ",
                    color = "Red",
                    alt = {
                        "OPTIM",
                        "PERFORMANCE",
                        "OPTIMIZE",
                    },
                },
                NOTE = {
                    icon = " ",
                    color = "Yellow",
                    alt = { "INFO" },
                },
                TEST = {
                    icon = "⏲ ",
                    color = "Blue",
                    alt = { "TESTING", "PASSED", "FAILED" },
                },
            },
            colors = {
                Red = { "GruvboxRed" },
                Green = { "GruvboxGreen" },
                Yellow = { "GruvboxYellow" },
                Blue = { "GruvboxBlue" },
            },
        },
    },
}
