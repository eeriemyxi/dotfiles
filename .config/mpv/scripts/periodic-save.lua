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

-- Save current time every 3s (except near the end)
local function save_position()
    local pos = mp.get_property_number("time-pos")
    local duration = mp.get_property_number("duration")
    if not pos or not duration then return end

    -- Do not save if we are within 2 seconds of the end
    if duration - pos < 2 then return end

    local title = mp.get_property("media-title") or mp.get_property("filename")
    if not title then return end

    local db = load_positions()
    db[title] = pos
    save_positions(db)
end

-- Try to restore when file loads and purge near-end history
mp.register_event("file-loaded", function()
    local title = mp.get_property("media-title") or mp.get_property("filename")
    if not title then return end

    local db = load_positions()
    if db[title] then
        local duration = mp.get_property_number("duration")

        -- If the saved position is within 2 seconds of the end, clear it from history
        if duration and (duration - db[title] < 2) then
            db[title] = nil
            save_positions(db)
            return
        end

        mp.set_property_number("time-pos", db[title])
        mp.osd_message("Resumed at " .. math.floor(db[title]) .. "s")
    end
end)

-- Run save every 3 seconds
mp.add_periodic_timer(3, save_position)
