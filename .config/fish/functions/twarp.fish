function twarp -d "Advanced Cloudflare Warp manager using argparse"
    # --- Configuration & Constants ---
    set -g twarp_pid_file "/tmp/twarp_timer.pid"
    set -g twarp_log_file "/tmp/twarp_activity.log"
    set -l DEFAULT_MINUTES 30

    # --- Helper Functions ---
    function _twarp_log
        set -l timestamp (date '+%Y-%m-%d %H:%M:%S')
        echo "[$timestamp] $argv" >> $twarp_log_file
    end

    function _twarp_notify
        set -l title $argv[1]
        set -l msg $argv[2]
        set -l time $argv[3]
        test -z "$time"; and set time 5000
        if type -q notify-send
            notify-send -t $time "$title" "$msg"
        end
        _twarp_log "Notification: $title - $msg"
    end

    # --- Dependency Check ---
    if not type -q warp-cli
        set_color red; echo "Error: 'warp-cli' is not installed or not in PATH."; set_color normal
        return 1
    end

    # --- Argument Parsing ---
    # argparse will extract valid flags and leave any positional arguments in $argv
    argparse --name=twarp 'h/help' 's/stop' 'k/keep' 'S/status' 'l/log' 't/time=' -- $argv
    or return 1 # Exits automatically if user passes an invalid flag

    # Legacy Subcommand Support: 
    # If a user types `twarp stop` instead of `twarp --stop`, translate it to the flag
    if test (count $argv) -gt 0
        switch "$argv[1]"
            case "stop" "disconnect"
                set _flag_stop 1
            case "keep"
                set _flag_keep 1
            case "status"
                set _flag_status 1
            case "log"
                set _flag_log 1
            case "help"
                set _flag_help 1
        end
    end

    # --- Command Routing ---
    if set -q _flag_help
        set_color cyan; echo "Twarp - Cloudflare Warp Auto-Manager"; set_color normal
        echo "Usage: twarp [MINUTES] [OPTIONS]"
        echo ""
        echo "Options:"
        echo "  -t, --time <min>   Set auto-disconnect timer in minutes (default: $DEFAULT_MINUTES)"
        echo "  -s, --stop         Disconnect Warp immediately and kill active timer"
        echo "  -k, --keep         Kill the disconnect timer, but stay connected to Warp"
        echo "  -S, --status       Show current Warp status and timer state"
        echo "  -l, --log          View the recent activity log"
        echo "  -h, --help         Show this help message"
        echo ""
        echo "Examples:"
        echo "  twarp              (Connects for 30 mins)"
        echo "  twarp 45           (Connects for 45 mins using positional argument)"
        echo "  twarp -t 120       (Connects for 120 mins using strict flag)"
        echo "  twarp --stop       (Disconnects instantly)"
        return 0
    end

    if set -q _flag_log
        if test -f $twarp_log_file
            tail -n 20 $twarp_log_file
        else
            echo "No log file found at $twarp_log_file"
        end
        return 0
    end

    if set -q _flag_status
        set_color yellow; echo "--- Warp CLI Status ---"; set_color normal
        warp-cli status
        echo ""
        set_color yellow; echo "--- Timer Status ---"; set_color normal
        if test -f $twarp_pid_file
            set -l pid (cat $twarp_pid_file)
            if kill -0 $pid 2>/dev/null
                set_color green; echo "Active timer running (PID: $pid)."; set_color normal
            else
                echo "Stale timer file found. Cleaning up..."
                rm -f $twarp_pid_file
            end
        else
            echo "No auto-disconnect timer is currently active."
        end
        return 0
    end

    if set -q _flag_stop
        echo "Stopping Warp connection..."
        warp-cli disconnect > /dev/null
        _twarp_notify "Warp CLI" "Manual disconnect initiated."
        
        if test -f $twarp_pid_file
            set -l pid (cat $twarp_pid_file)
            kill $pid 2>/dev/null
            rm -f $twarp_pid_file
            echo "Cancelled pending auto-disconnect timer."
        end
        return 0
    end

    if set -q _flag_keep
        if test -f $twarp_pid_file
            set -l pid (cat $twarp_pid_file)
            kill $pid 2>/dev/null
            rm -f $twarp_pid_file
            set_color green; echo "Timer cancelled. Warp will stay connected."; set_color normal
            _twarp_notify "Warp CLI" "Timer cancelled. Connection preserved."
        else
            echo "No active timer found."
        end
        return 0
    end

    # --- Timer Logic & Validation ---
    set -l wait_time $DEFAULT_MINUTES
    
    # Check if time was passed via flag (-t 45) or positional argument (twarp 45)
    if set -q _flag_time
        set wait_time $_flag_time
    else if test (count $argv) -gt 0
        if string match -qr '^[0-9]+$' "$argv[1]"
            set wait_time $argv[1]
        else
            set_color red; echo "Error: Invalid argument '$argv[1]'. Check 'twarp --help'."; set_color normal
            return 1
        end
    end

    # Check for duplicate timer
    if test -f $twarp_pid_file
        set -l old_pid (cat $twarp_pid_file)
        if kill -0 $old_pid 2>/dev/null
            set_color yellow; echo "A timer is already running. Replacing it..."; set_color normal
            kill $old_pid
        end
        rm -f $twarp_pid_file
    end

    # --- Execution ---
    set -l sleep_seconds (math "$wait_time * 60")
    
    echo "Connecting to Cloudflare Warp..."
    warp-cli connect > /dev/null
    _twarp_log "Connected manually. Timer set for $wait_time minutes."
    
    # Run the sleep and disconnect in a background subshell
    env WARP_TIME=$wait_time WARP_SEC=$sleep_seconds sh -c '
    sleep $WARP_SEC
    warp-cli disconnect > /dev/null
    if command -v notify-send >/dev/null; then
    notify-send -t 10000 "Warp CLI" "Auto-disconnected after $WARP_TIME minutes."
    fi
    echo "[$(date "+%Y-%m-%d %H:%M:%S")] Auto-disconnected." >> /tmp/twarp_activity.log
    rm -f /tmp/twarp_timer.pid
    ' &
    
    # Save the PID so we can kill it later with --keep or --stop
    set -l bg_pid $last_pid
    echo $bg_pid > $twarp_pid_file
    
    disown $bg_pid

    set_color green
    echo "Warp connection established."
    echo "Auto-disconnect scheduled in $wait_time minute(s)."
    set_color normal
    _twarp_notify "Warp CLI" "Connected. Will disconnect in $wait_time min." 5000
end
