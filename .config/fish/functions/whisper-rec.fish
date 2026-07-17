function whisper-rec
    set -l model_path "$HOME/.local/share/com.bradenwong.whispering/models/whisper/ggml-large-v3-turbo-q5_0.bin"
    set -l server_bin "$HOME/.local/bin/whisper-server"
    set -l port 8080

    set -l repl_mode 0
    if contains -- --repl $argv
        set repl_mode 1
    end

    # Initialize memory daemon if missing
    if not curl -s http://localhost:$port >/dev/null 2>&1
        echo "🚀 Loading model into RAM daemon..."
        $server_bin -m $model_path -t 6 -l en --port $port >/dev/null 2>&1 &
        set -g WHISPER_SERVER_PID $last_pid

        while true
            curl -s http://localhost:$port >/dev/null 2>&1
            if test $status -ne 7
                break
            end
            sleep 0.1
        end
        echo "✨ Model resident in memory and ready."
    end

    while true
        # --- STAGE 1: IDLE / ARMING (Only applies to REPL mode loop steps) ---
        if test $repl_mode -eq 1
            echo "💤 Idle. Press [Spacebar] to record next slice | [q] to terminate daemon"
            set -l arm_key ""
            while true
                read -l -n 1 -s key
                if test "$key" = " " -o "$key" = "q"
                    set arm_key "$key"
                    break
                end
            end

            if test "$arm_key" = "q"
                echo "❌ Exiting REPL mode. Purging memory daemon..."
                kill $WHISPER_SERVER_PID 2>/dev/null
                pkill -f whisper-server >/dev/null 2>&1
                break
            end
        end

        # --- STAGE 2: ACTIVE CAPTURE ---
        set -l tmp_raw (mktemp -p /dev/shm --suffix=.wav)
        set -l tmp_opus (mktemp -p /dev/shm --suffix=.opus)
        set -l tmp_ready (mktemp -p /dev/shm --suffix=.wav)

        ffmpeg -nostdin -y -f pulse -i default $tmp_raw >/dev/null 2>&1 &
        set -l ffmpeg_pid $last_pid

        echo "🎙️  Recording..."
        echo "   [Spacebar] -> Stop & Transcribe"
        echo "   [q]        -> Cancel current slice"

        set -l user_action "process"
        while true
            read -l -n 1 -s key
            if test "$key" = " "
                break
            else if test "$key" = "q"
                set user_action "cancel"
                break
            end
        end

        # Hard interrupt ffmpeg to close file descriptors safely
        kill -2 $ffmpeg_pid
        wait $ffmpeg_pid 2>/dev/null

        if test "$user_action" = "cancel"
            echo "🗑️  Slice dropped."
            rm -f $tmp_raw $tmp_opus $tmp_ready
            if test $repl_mode -eq 0; kill $WHISPER_SERVER_PID 2>/dev/null; break; end
            continue
        end

        if not test -s $tmp_raw
            echo "⚠️ Error: Capture buffer empty."
            rm -f $tmp_raw $tmp_opus $tmp_ready
            if test $repl_mode -eq 0; kill $WHISPER_SERVER_PID 2>/dev/null; break; end
            continue
        end

        # --- STAGE 3: EXECUTE INFERENCE ---
        echo "⚡ Running parallel DSP filters..."
        ffmpeg -y -threads 6 -i $tmp_raw -af "silenceremove=start_periods=1:start_duration=0.1:start_threshold=-50dB:detection=peak,aformat=sample_fmts=s16:sample_rates=16000:channel_layouts=mono" -c:a libopus -b:a 32k -ar 16000 -ac 1 -compression_level 10 $tmp_opus >/dev/null 2>&1
        ffmpeg -y -threads 6 -i $tmp_opus -ar 16000 -ac 1 -c:a pcm_s16le $tmp_ready >/dev/null 2>&1

        if not test -s $tmp_ready
            echo "⚠️ Error: Track stripped by silence threshold."
            rm -f $tmp_raw $tmp_opus $tmp_ready
            if test $repl_mode -eq 0; kill $WHISPER_SERVER_PID 2>/dev/null; break; end
            continue
        end

        echo "🔮 Blistering inference via local API..."
        set -l json_res (curl -s -F "file=@$tmp_ready" http://localhost:$port/inference)
        set -l text (echo $json_res | python3 -c "import sys, json; print(json.load(sys.stdin).get('text', '').strip())")

        if test -n "$text"
            echo "$text" | wl-copy
            echo -e "📋 Copied to clipboard:\n\"$text\"\n"
        else
            echo "⚠️ Transcription execution returned empty payload."
        end

        # Clear active cycle storage states from shared RAM
        rm -f $tmp_raw $tmp_opus $tmp_ready

        # Standard non-REPL execution tears down daemon instantly
        if test $repl_mode -eq 0
            kill $WHISPER_SERVER_PID 2>/dev/null
            break
        end
    end
end
