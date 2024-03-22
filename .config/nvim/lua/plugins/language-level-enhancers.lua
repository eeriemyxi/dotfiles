return {
    { "alaviss/nim.nvim", event = {"BufRead *.nimble", "BufRead *.nim"},
    init = function() 
        vim.cmd [[
            autocmd FileType nim setlocal shiftwidth=4 softtabstop=4 expandtab
        ]]
    end
}
}
