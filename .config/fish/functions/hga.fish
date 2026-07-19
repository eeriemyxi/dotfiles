function play_sound
    sleep 0.2
    pw-play '/home/myxi/Music/Windows 11 startup sound.wav' &
end

function hga --description 'Toggle audio using native PipeWire ports for Blue rear jack with dynamic index routing'
    # Parse options using Fish native argparse
    argparse 'f/fix' -- $argv
    or return 1

    # 1. Dynamically locate which ALSA card index currently possesses the 'Line Out' channel
    set -l alsa_card ""
    for c in 0 1 2 3
        if amixer -c $c get 'Line Out' >/dev/null 2>&1
            set alsa_card $c
            break
        end
    end

    # Fail gracefully if the onboard analog card is missing or sleeping
    if test -z "$alsa_card"
        echo "❌ Error: Motherboard sound card with 'Line Out' control could not be resolved."
        return 1
    end

    # Inline fail-safe: Enforce Auto-Mute disabled on the correct index every time the toggle runs
    amixer -c $alsa_card sset 'Auto-Mute Mode' Disabled > /dev/null 2>&1

    # 2. Find the hardware analog stereo sink matching pavucontrol
    set -l target_sink (pactl list sinks short | string match -r 'alsa_output\S+analog-stereo')[1]
    if test -z "$target_sink"
        set target_sink "@DEFAULT_SINK@"
    end

    # 3. Define target ports based on pactl output
    set -l speaker_port "analog-output-lineout"
    set -l headphone_port "analog-output-headphones-2" # Target for Blue rear jack

    # Handle startup alignment fix
    if set -q _flag_fix
        # Determine current active port from PipeWire state
        set -l active_port (pactl list sinks | string match -r 'Active Port: \S+' | string replace 'Active Port: ' '')[1]

        if test "$active_port" = "$headphone_port"
            amixer -c $alsa_card sset 'Line Out' mute > /dev/null
            pactl set-sink-port $target_sink $headphone_port > /dev/null
            echo "🔧 Startup Fix: Headset synchronized [Line Out Muted]"
        else
            # Default fallback to Speakers if port is lineout or indeterminate
            amixer -c $alsa_card sset 'Line Out' unmute > /dev/null
            pactl set-sink-port $target_sink $speaker_port > /dev/null
            echo "🔧 Startup Fix: Speakers synchronized [Line Out Unmuted]"
        end
        return 0
    end

    # 4. Toggle based on whether the dynamically resolved ALSA card's Line Out is unmuted
    if amixer -c $alsa_card get 'Line Out' | string match -r -q '\[on\]'
        # --- SWITCHING TO HEADPHONES ONLY ---
        amixer -c $alsa_card sset 'Line Out' mute > /dev/null
        pactl set-sink-port $target_sink $headphone_port > /dev/null

        set -l vol (pactl get-sink-volume $target_sink | string match -r '\d+%' | string trim -c '%')[1]
        echo "🎧 Headset Active [Blue Jack] (pavucontrol volume: $vol%)"
        play_sound
    else
        # --- SWITCHING TO SPEAKERS ONLY ---
        amixer -c $alsa_card sset 'Line Out' unmute > /dev/null
        pactl set-sink-port $target_sink $speaker_port > /dev/null

        set -l vol (pactl get-sink-volume $target_sink | string match -r '\d+%' | string trim -c '%')[1]
        echo "🔊 Speakers Active [Green Jack] (pavucontrol volume: $vol%)"
        play_sound
    end
end
