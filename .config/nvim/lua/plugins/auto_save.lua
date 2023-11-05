return {
    "okuuva/auto-save.nvim",
    cmd = "ASToggle",
    cond = not vim.g.started_by_firenvim,
    event = { "InsertLeave", "TextChanged" },
}
