return {
    "ray-x/lsp_signature.nvim",
    event = "VeryLazy",
    opts = {
        handler_opts = {
            border = "none",
        },
        move_cursor_key = "<A-c>",
    },
    config = function(_, opts)
        require("lsp_signature").setup(opts)

        local function escape_term_codes(str)
            return vim.api.nvim_replace_termcodes(
                str,
                true,
                false,
                true
            )
        end

        local function is_float_open(window_id)
            return window_id
                and window_id ~= 0
                and vim.api.nvim_win_is_valid(window_id)
        end

        local function scroll_float(mapping)
            -- Using the global config of the lsp_signature plugin
            local window_id = _G._LSP_SIG_CFG.winnr

            if is_float_open(window_id) then
                vim.fn.win_execute(
                    window_id,
                    "normal! " .. mapping
                )
            end
        end

        local map = vim.keymap.set
        local opts = { noremap = true, silent = true }
        local scroll_up_mapping = escape_term_codes("<c-u>")
        local scroll_down_mapping =
            escape_term_codes("<c-d>")
        map("i", "<c-u>", function()
            scroll_float(scroll_up_mapping)
        end, opts)
        map("i", "<c-d>", function()
            scroll_float(scroll_down_mapping)
        end, opts)
    end,
}
