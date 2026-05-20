function tparse --description 'Comprehensive date parser, timezone converter, and temporal analyzer'
    # ==============================================================================
    # 1. ARGUMENT PARSING & SETUP
    # ==============================================================================
    set -l options 'h/help' 'i/input-tz=' 'o/output-tz=' 'c/no-color'
    argparse $options -- $argv
    or return 1

    if set -q _flag_help
        echo "Usage: tparse [OPTIONS] [DATE_STRING]"
        echo "Parses date inputs and presents a massive temporal overview."
        echo ""
        echo "Options:"
        echo "  -h, --help           Show this help message"
        echo "  -i, --input-tz=TZ    Specify input timezone (Default: UTC)"
        echo "  -o, --output-tz=TZ   Specify output timezone (Default: Asia/Kolkata)"
        echo "  -c, --no-color       Disable colorized output"
        return 0
    end

    set -l input_tz "UTC"
    set -l output_tz "Asia/Kolkata"

    if set -q _flag_input_tz; set input_tz $_flag_input_tz; end
    if set -q _flag_output_tz; set output_tz $_flag_output_tz; end

    # Aesthetic Palette
    set -l c_reset  (set_color normal)
    set -l c_border (set_color 666)
    set -l c_head   (set_color -o blue)
    set -l c_cat    (set_color -o yellow)
    set -l c_key    (set_color cyan)
    set -l c_val    (set_color green)
    set -l c_err    (set_color red)
    set -l c_dim    (set_color 555)

    if set -q _flag_no_color
        set c_reset ""; set c_border ""; set c_head ""; set c_cat ""
        set c_key ""; set c_val ""; set c_err ""; set c_dim ""
    end

    set -l date_str (string join " " $argv)
    if test -z "$date_str"; set date_str "now"; end

    # ==============================================================================
    # 2. VALIDATION & PARSING (GNU Date)
    # ==============================================================================
    function _validate_tz -a tz -a c_err -a c_reset
        if test "$tz" = "UTC" -o "$tz" = "GMT"; return 0; end
        if not test -f "/usr/share/zoneinfo/$tz"
            echo -e "$c_err[Error] Invalid timezone: $tz$c_reset"
            return 1
        end
        return 0
    end

    _validate_tz $input_tz $c_err $c_reset; or return 1
    _validate_tz $output_tz $c_err $c_reset; or return 1

    # Extract Epoch (Seconds + Nanoseconds)
    set -l epoch (env TZ=$input_tz date -d "$date_str" +"%s.%N" 2>/dev/null)
    if test $status -ne 0
        set -l cleaned_str (string replace -a "T" " " $date_str)
        set epoch (env TZ=$input_tz date -d "$cleaned_str" +"%s.%N" 2>/dev/null)
        if test $status -ne 0
            echo -e "$c_err[Error] Unrecognized date format: '$date_str'$c_reset"
            return 1
        end
    end

    set -l epoch_sec (string split -f 1 "." $epoch)
    set -l epoch_nano (string split -f 2 "." $epoch)
    if test -z "$epoch_nano"; set epoch_nano "000000000"; end

    # ==============================================================================
    # 3. DATA EXTRACTION & CALCULATIONS
    # ==============================================================================
    
    # --- Time Formats ---
    # By calling date directly with quoted format strings, we avoid eval parsing errors
    set -l out_std      (env TZ=$output_tz date -d "@$epoch_sec" "+%Y-%m-%d %H:%M:%S %Z (%:z)")
    set -l out_12hr     (env TZ=$output_tz date -d "@$epoch_sec" "+%I:%M:%S %p")
    set -l out_24hr     (env TZ=$output_tz date -d "@$epoch_sec" "+%H:%M:%S")
    set -l out_human    (env TZ=$output_tz date -d "@$epoch_sec" "+%A, %d %B %Y")
    
    # --- Time of Day Context ---
    set -l hour24 (env TZ=$output_tz date -d "@$epoch_sec" "+%H")
    set -l time_context ""
    if test $hour24 -ge 5 -a $hour24 -lt 12; set time_context "Morning"
    else if test $hour24 -ge 12 -a $hour24 -lt 17; set time_context "Afternoon"
    else if test $hour24 -ge 17 -a $hour24 -lt 21; set time_context "Evening"
    else; set time_context "Night"
    end

    # --- Standards ---
    set -l out_iso  (env TZ=$output_tz date -d "@$epoch_sec" "--iso-8601=seconds")
    set -l out_rfc  (env TZ=$output_tz date -d "@$epoch_sec" "--rfc-3339=seconds")

    # --- Unix Epochs ---
    set -l nano_trimmed (string sub -l 3 $epoch_nano)
    set -l micro_trimmed (string sub -l 6 $epoch_nano)
    set -l ep_ms (math "$epoch_sec * 1000 + $nano_trimmed")
    set -l ep_us (math "$epoch_sec * 1000000 + $micro_trimmed")
    set -l ep_ns "$epoch_sec$epoch_nano"

    # --- Calendar Details ---
    set -l c_dow  (env TZ=$output_tz date -d "@$epoch_sec" "+%A")
    set -l c_dnum (env TZ=$output_tz date -d "@$epoch_sec" "+%u") # 1-7
    set -l c_doy  (env TZ=$output_tz date -d "@$epoch_sec" "+%j") # 1-366
    set -l c_woy  (env TZ=$output_tz date -d "@$epoch_sec" "+%V") # Week of year
    
    # Calculate Quarter
    set -l c_month (env TZ=$output_tz date -d "@$epoch_sec" "+%m" | string trim -l -c 0)
    # math fails if c_month is completely empty (e.g., failed parsing), fallback to 1
    if test -z "$c_month"; set c_month 1; end
    set -l c_quarter (math "floor(($c_month - 1) / 3) + 1")

    # Leap Year Check
    set -l c_year (env TZ=$output_tz date -d "@$epoch_sec" "+%Y")
    set -l is_leap "No"
    if test (math "$c_year % 4") -eq 0 -a \( (math "$c_year % 100") -ne 0 -o (math "$c_year % 400") -eq 0 \)
        set is_leap "Yes"
    end
    set -l days_in_year 365
    if test "$is_leap" = "Yes"; set days_in_year 366; end

    # --- Timezones ---
    set -l tz_utc   (env TZ="UTC" date -d "@$epoch_sec" "+%Y-%m-%d %H:%M:%S UTC")
    set -l tz_local (date -d "@$epoch_sec" "+%Y-%m-%d %H:%M:%S %Z")
    set -l tz_src   (env TZ=$input_tz date -d "@$epoch_sec" "+%Y-%m-%d %H:%M:%S %Z")

    # --- Relative Time Math ---
    set -l now_sec (date +%s)
    set -l diff_sec (math "$epoch_sec - $now_sec")
    set -l diff_dir "In the Past"
    if test $diff_sec -gt 0; set diff_dir "In the Future"; end
    if test $diff_sec -eq 0; set diff_dir "Right Now"; end
    
    set -l abs_diff (math "abs($diff_sec)")
    set -l r_days  (math "floor($abs_diff / 86400)")
    set -l r_hours (math "floor(($abs_diff % 86400) / 3600)")
    set -l r_mins  (math "floor(($abs_diff % 3600) / 60)")
    set -l r_secs  (math "$abs_diff % 60")

    # ==============================================================================
    # 4. RENDER UI
    # ==============================================================================
    function _row -a key val dim_val
        printf "  %s%-17s%s : %s%s%s" "$c_key" "$key" "$c_reset" "$c_val" "$val" "$c_reset"
        if test -n "$dim_val"
            printf " %s%s%s" "$c_dim" "$dim_val" "$c_reset"
        end
        echo ""
    end

    echo -e "\n$c_border============================================================$c_reset"
    echo -e "$c_head TEMPORAL OVERVIEW$c_reset"
    echo -e "$c_border============================================================$c_reset"

    # Using printf cleanly separates the variables from the brackets
    printf "\n%s[ Query Context ]%s\n" "$c_cat" "$c_reset"
    _row "Raw Input" "'$date_str'"
    _row "Assumed Source" "$input_tz"

    printf "\n%s[ Target Timezone: %s ]%s\n" "$c_cat" "$output_tz" "$c_reset"
    _row "Standard" "$out_std"
    _row "Human Readable" "$out_human"
    _row "12-Hour Clock" "$out_12hr" "($time_context)"
    _row "24-Hour Clock" "$out_24hr"

    printf "\n%s[ Global References ]%s\n" "$c_cat" "$c_reset"
    _row "Local System" "$tz_local" "(Your Machine)"
    _row "UTC / GMT" "$tz_utc"
    if test "$input_tz" != "UTC" -a "$input_tz" != "$output_tz"
        _row "Source TZ" "$tz_src"
    end

    printf "\n%s[ Calendar Details ]%s\n" "$c_cat" "$c_reset"
    _row "Day of Week" "$c_dow" "(Day $c_dnum of 7)"
    _row "Day of Year" "$c_doy" "(of $days_in_year)"
    _row "Week of Year" "Week $c_woy"
    _row "Quarter" "Q$c_quarter"
    _row "Leap Year" "$is_leap"

    printf "\n%s[ Developer Standards & Epochs ]%s\n" "$c_cat" "$c_reset"
    _row "ISO 8601" "$out_iso"
    _row "RFC 3339" "$out_rfc"
    _row "Unix Seconds" "$epoch_sec"
    _row "Unix Milli" "$ep_ms"
    _row "Unix Micro" "$ep_us"
    _row "Unix Nano" "$ep_ns"

    printf "\n%s[ Relative Timeline ]%s\n" "$c_cat" "$c_reset"
    _row "Temporal Status" "$diff_dir"
    if test $abs_diff -gt 0
        _row "Difference" "$r_days days, $r_hours hrs, $r_mins mins, $r_secs secs"
        _row "Total Seconds" "(±) $abs_diff"
    end

    echo -e "$c_border============================================================$c_reset\n"
end
