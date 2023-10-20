vim.opt.backup = true
vim.opt.backupdir = "./.backup"
local backup_records = {}

function backup_file(info)
    local filename = info["file"]
    local file_path = vim.fn.expand("#" .. info["buf"] .. ":p")
    local parent_path = vim.fs.dirname(file_path)
    vim.loop.fs_mkdir(parent_path .. "/.backup", 448)

    print(
        string.format(
            "Checking if backup is needed for `%s`...",
            filename
        )
    )

    local record = backup_records[filename]
    if record == nil then
        backup_records[filename] = os.time() - 900
        record = backup_records[filename]
    end

    if os.time() - record >= 900 then
        vim.opt.backup = true
        backup_records[filename] = os.time()
        print(string.format("Backing up %s...", filename))
    else
        vim.opt.backup = false
    end
end

vim.api.nvim_create_autocmd({ "BufWritePre" }, {
    callback = function(info)
        local formatted_date = os.date("%Y-%m-%d--%H:%M:%S")
        vim.opt.bex = string.format("-%s~", formatted_date)

        backup_file(info)
    end,
})
