return {
    {
        "morhetz/gruvbox",
        priority = 1000,
        config = function()
            vim.cmd.colorscheme("gruvbox")
            vim.cmd("hi Normal guibg=#242221")
        end,
    },
}
