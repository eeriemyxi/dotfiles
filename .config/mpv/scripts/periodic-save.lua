-- local utils = require 'mp.utils'
-- 
-- local function save_position()
--     mp.commandv("write-watch-later-config")
-- end
-- 
-- local interval = 3
-- mp.add_periodic_timer(interval, save_position)

local utils = require 'mp.utils'

-- Where we'll store positions
local save_file = (os.getenv("HOME") or os.getenv("USERPROFILE")) .. "/.config/mpv/stream_positions.json"

-- Read saved positions
local function load_positions()
    local f = io.open(save_file, "r")
    if not f then return {} end
    local content = f:read("*a")
    f:close()
    local ok, data = pcall(utils.parse_json, content)
    if ok and data then return data else return {} end
end

-- Write saved positions
local function save_positions(tbl)
    local f = io.open(save_file, "w+")
    if not f then return end
    f:write(utils.format_json(tbl))
    f:close()
end

-- Save current time every 3s
local function save_position()
    local pos = mp.get_property_number("time-pos")
    if not pos then return end
    local title = mp.get_property("media-title") or mp.get_property("filename")
    if not title then return end

    local db = load_positions()
    db[title] = pos
    save_positions(db)
end

-- Try to restore when file loads
mp.register_event("file-loaded", function()
    local title = mp.get_property("media-title") or mp.get_property("filename")
    if not title then return end
    local db = load_positions()
    if db[title] then
        mp.set_property_number("time-pos", db[title])
        mp.osd_message("Resumed at " .. math.floor(db[title]) .. "s")
    end
end)

-- Run save every 3 seconds
mp.add_periodic_timer(3, save_position)
