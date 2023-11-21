return {
    "kylechui/nvim-surround",
    version = "*",
    event = "VeryLazy",
    config = function()
        require("nvim-surround").setup({
            keymaps = {
                change = "cs",
                change_line = "cS",
                delete = "ds",
                insert = "<C-g>s",
                insert_line = "<C-g>S",
                normal = "ys",
                normal_cur = "yss",
                normal_cur_line = "ySS",
                normal_line = "yS",
                visual = "k",
                visual_line = "k",
            },
        })
    end,
}
