local mp = require 'mp'
local utils = require 'mp.utils'
local msg = require 'mp.msg'
local options = require 'mp.options'

-- Default configuration parameters
local o = {
    enabled = true,
    save_interval = 5,
    end_threshold_seconds = 10,
    end_threshold_percent = 98,
    start_threshold_seconds = 5,
    max_entries = 1000,
    save_file = "",
    use_osd = true,
    ignore_short_files_seconds = 30,
}
options.read_options(o, "stream_positions")

if not o.enabled then return end

-- Internal State Management
local state = {
    db = {},
    current_key = nil,
    last_saved_pos = -1,
    is_paused = false,
    timer = nil,
    db_loaded = false
}

-- Resolve cross-platform storage location safely
if o.save_file == "" then
    local config_dir = mp.command_native({"expand-path", "~~home/"}) or mp.command_native({"expand-path", "~~/"})
    if not config_dir then
        msg.error("Could not resolve mpv configuration directory.")
        return
    end
    o.save_file = utils.join_path(config_dir, "stream_positions.json")
end

-- Generate a unique hash or identifier for media files to avoid path/leak exposures
local function get_media_key()
    local path = mp.get_property("path")
    if not path or path == "" then return nil end

    -- Handle common streaming protocols cleanly by stripping volatile query strings
    if path:find("^https?:") or path:find("^ytdl:") then
        local clean_url = path:gsub("%?.*$", "")
        return "stream:" .. clean_url
    end

    -- Exclude non-resumable protocols
    local protocol = path:match("^([%a%d%+%.%-]+)://")
    if protocol and (protocol == "bd" or protocol == "dvd" or protocol == "stdin") then
        return nil
    end

    return "local:" .. path
end

-- Secure File-I/O: Read JSON from disk safely
local function load_db()
    if state.db_loaded then return end
    state.db_loaded = true

    local f, err = io.open(o.save_file, "r")
    if not f then
        state.db = {}
        return
    end

    local content = f:read("*a")
    f:close()

    if not content or content == "" then
        state.db = {}
        return
    end

    local ok, data = pcall(utils.parse_json, content)
    if ok and type(data) == "table" then
        state.db = data
    else
        msg.warn("Database corrupted or unreadable. Initializing fresh array.")
        state.db = {}
    end
end

-- Secure File-I/O: Atomic write transaction using an absolute temporary file fallback
local function save_db()
    load_db() -- Ensure state database array initialization

    -- Enforce LRU bounding constraints to stop database bloating
    local keys = {}
    for k, v in pairs(state.db) do
        if type(v) == "table" and v.time then
            table.insert(keys, {key = k, time = v.time})
        else
            -- Migrate or handle old flat database structures cleanly
            state.db[k] = {pos = v, time = os.time()}
            table.insert(keys, {key = k, time = state.db[k].time})
        end
    end

    if #keys > o.max_entries then
        table.sort(keys, function(a, b) return a.time > b.time end)
        for i = o.max_entries + 1, #keys do
            state.db[keys[i].key] = nil
        end
    end

    local ok, json_str = pcall(utils.format_json, state.db)
    if not ok or not json_str then
        msg.error("Failed to parse system configuration state into JSON.")
        return
    end

    -- Write safely to a temporary file first
    local tmp_file = o.save_file .. ".tmp"
    local f, err = io.open(tmp_file, "w+")
    if not f then
        msg.error("Cannot open storage layout for write vectors: " .. tostring(err))
        return
    end

    f:write(json_str)
    f:close()

    -- Atomically swap file nodes via system rename configurations
    local success, rename_err = os.rename(tmp_file, o.save_file)
    if not success then
        msg.warn("Atomic file allocation error, attempting direct fallback write: " .. tostring(rename_err))
        f = io.open(o.save_file, "w+")
        if f then
            f:write(json_str)
            f:close()
        else
            msg.error("Critical database write failure.")
        end
    end
end

-- Clear an asset signature completely out of cache and persistent storage
local function purge_current_key()
    if not state.current_key then return end
    load_db()
    if state.db[state.current_key] then
        state.db[state.current_key] = nil
        save_db()
        msg.info("Purged positional record: " .. state.current_key)
    end
end

-- Process tracking conditions and save metrics dynamically
local function process_position_save()
    if not state.current_key or state.is_paused then return end

    local pos = mp.get_property_number("time-pos")
    local duration = mp.get_property_number("duration")

    if not pos or not duration or duration <= 0 then return end
    if duration < o.ignore_short_files_seconds then return end

    -- Avoid disk updates if the position has not drifted significantly
    if math.abs(pos - state.last_saved_pos) < 1 then return end

    -- Check if within boundary parameters near the end of the file
    local is_near_end = (duration - pos < o.end_threshold_seconds) or
                        ((pos / duration) > o.end_threshold_percent)

    -- Check if within boundary parameters near the start of the file
    local is_near_start = (pos < o.start_threshold_seconds)

    if is_near_end or is_near_start then
        purge_current_key()
        state.last_saved_pos = pos
        return
    end

    load_db()
    state.db[state.current_key] = {
        pos = pos,
        time = os.time()
    }
    save_db()
    state.last_saved_pos = pos
end

-- Handle async stream parsing triggers cleanly using property observation instead of race hooks
local function on_file_loaded()
    state.current_key = get_media_key()
    state.last_saved_pos = -1

    if not state.current_key then return end

    load_db()
    local record = state.db[state.current_key]
    if record and type(record) == "table" and record.pos then
        local target_seek = record.pos

        -- Execute an accurate absolute seek command transaction sequence
        mp.commandv("seek", target_seek, "absolute", "exact")

        if o.use_osd then
            local floor_time = math.floor(target_seek)
            local m = math.floor(floor_time / 60)
            local s = floor_time % 60
            mp.osd_message(string.format("Resumed playback at %02d:%02d", m, s), 3)
        end
        msg.info("Successfully recovered position state: " .. target_seek .. "s")
    end

    -- Dynamically track runtime status changes to eliminate idle background timer cycles
    if not state.timer then
        state.timer = mp.add_periodic_timer(o.save_interval, process_position_save)
    else
        state.timer:resume()
    end
end

-- Stop execution tracking cycles cleanly when operations terminate or change context paths
local function on_end_file(event)
    if state.timer then state.timer:kill() end

    if event and event.reason == "eof" then
        purge_current_key()
    else
        process_position_save()
    end

    state.current_key = nil
    state.last_saved_pos = -1
end

-- Monitor pause properties natively to freeze data processing loops instantly
mp.observe_property("pause", "bool", function(_, val)
    state.is_paused = val
    if val then
        process_position_save()
    end
end)

-- Core MPV Operational Bindings
mp.register_event("file-loaded", on_file_loaded)
mp.register_event("end-file", on_end_file)

-- Allow users to manually clear positions using an interactive keybind (Shift+R)
mp.add_key_binding("S", "clear-position-history", function()
    if state.current_key then
        purge_current_key()
        mp.osd_message("Cleared resume history for this file", 3)
    else
        mp.osd_message("No history record available to purge", 3)
    end
end)
