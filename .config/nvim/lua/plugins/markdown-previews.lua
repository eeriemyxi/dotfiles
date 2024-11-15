return {
    {
        "iamcco/markdown-preview.nvim",
        cmd = {
            "MarkdownPreviewToggle",
            "MarkdownPreview",
            "MarkdownPreviewStop",
        },
        build = "cd app && yarn install",
        init = function()
            vim.g.mkdp_filetypes = { "markdown" }
            vim.g.mkdp_echo_preview_url = 1
        end,
        ft = { "markdown" },
    },
    -- {
    --     "eeriemyxi/illumination",
    --     -- dir = "~/.illumination",
    --     build = "/bin/sh install.sh",
    -- },
}
