-- return { "justinmk/vim-sneak", event = "VeryLazy" }
return {
    "ggandor/leap.nvim",
    config = function()
        leap = require("leap")
        -- leap.opts.safe_labels = 'qptSFNLHMUGTA?'
        leap.opts.safe_labels = ""
        leap.opts.max_phase_one_targets = 0
        leap.opts.labels =
            "sfjklodwmbuyvrgtaqpcxz/SFNJKLHODWEIMBUYVRGTAQPCXZ?"
        leap.add_default_mappings()
        leap.add_repeat_mappings(";", ",", {
            -- False by default. If set to true, the keys will work like the
            -- native semicolon/comma, i.e., forward/backward is understood in
            -- relation to the last motion.
            relative_directions = true,
            -- By default, all modes are included.
            modes = { "n", "x", "o" },
        })
    end,
}
