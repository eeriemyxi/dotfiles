return {
    "romgrk/barbar.nvim",
    dependencies = {
        "lewis6991/gitsigns.nvim", -- OPTIONAL: for git status
        "nvim-tree/nvim-web-devicons", -- OPTIONAL: for file icons
    },
    opts = {
        letters = "arstneiokcvm",
    },
    cond = not vim.g.started_by_firenvim,
    init = function()
        local map = vim.api.nvim_set_keymap
        local opts = { noremap = true, silent = true }

        vim.g.barbar_auto_setup = false
        -- map("n", "q", "<Nop>", opts)

        -- Move to previous/next
        map(
            "n",
            "<Space>th",
            "<Cmd>BufferPrevious<CR>",
            opts
        )
        map("n", "<Space>ti", "<Cmd>BufferNext<CR>", opts)
        -- Re-order to previous/next
        map(
            "n",
            "<Space>Th",
            "<Cmd>BufferMovePrevious<CR>",
            opts
        )
        map(
            "n",
            "<Space>Ti",
            "<Cmd>BufferMoveNext<CR>",
            opts
        )
        -- Goto buffer in position...
        map("n", "<A-1>", "<Cmd>BufferGoto 1<CR>", opts)
        map("n", "<A-2>", "<Cmd>BufferGoto 2<CR>", opts)
        map("n", "<A-3>", "<Cmd>BufferGoto 3<CR>", opts)
        map("n", "<A-4>", "<Cmd>BufferGoto 4<CR>", opts)
        map("n", "<A-5>", "<Cmd>BufferGoto 5<CR>", opts)
        map("n", "<A-6>", "<Cmd>BufferGoto 6<CR>", opts)
        map("n", "<A-7>", "<Cmd>BufferGoto 7<CR>", opts)
        map("n", "<A-8>", "<Cmd>BufferGoto 8<CR>", opts)
        map("n", "<A-9>", "<Cmd>BufferGoto 9<CR>", opts)
        map("n", "<A-0>", "<Cmd>BufferLast<CR>", opts)
        -- Pin/unpin buffer
        map("n", "<Space>tp", "<Cmd>BufferPin<CR>", opts)
        -- Close buffer
        map("n", "<Space>tc", "<Cmd>BufferClose<CR>", opts)
        -- Wipeout buffer
        --                 :BufferWipeout
        -- Close commands
        --                 :BufferCloseAllButCurrent
        --                 :BufferCloseAllButPinned
        --                 :BufferCloseAllButCurrentOrPinned
        --                 :BufferCloseBuffersLeft
        --                 :BufferCloseBuffersRight
        -- Magic buffer-picking mode
        map(
            "n",
            "<Space>t<Space>",
            "<Cmd>BufferPick<CR>",
            opts
        )
        -- Sort automatically by...
        -- map(
        --     "n",
        --     "<Space>bb",
        --     "<Cmd>BufferOrderByBufferNumber<CR>",
        --     opts
        -- )
        -- map(
        --     "n",
        --     "<Space>bd",
        --     "<Cmd>BufferOrderByDirectory<CR>",
        --     opts
        -- )
        -- map(
        --     "n",
        --     "<Space>bl",
        --     "<Cmd>BufferOrderByLanguage<CR>",
        --     opts
        -- )
        -- map(
        --     "n",
        --     "<Space>bw",
        --     "<Cmd>BufferOrderByWindowNumber<CR>",
        --     opts
        -- )
        --
        -- Other:
        -- :BarbarEnable - enables barbar (enabled by default)
        -- :BarbarDisable - very bad command, should never be used
    end,
}
