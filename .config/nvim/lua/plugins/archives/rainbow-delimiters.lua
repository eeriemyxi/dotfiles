return {
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
}
