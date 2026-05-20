function typeswift --description 'Robust Python HTTP server wrapper for TypeSwift'
    # =========================================================================
    # 1. Argument Parsing setup
    # =========================================================================
    argparse 'h/help' 'p/port=' 'd/dir=' 'n/no-open' 'b/bind=' -- $argv
    if test $status -ne 0
        set_color red; echo "Error: Invalid arguments provided."
        set_color normal; echo "Run 'typeswift --help' for usage instructions."
        return 1
    end

    # =========================================================================
    # 2. Help Menu
    # =========================================================================
    if set -q _flag_help
        set_color cyan; echo "TypeSwift Dev Server"
        set_color normal; echo "  A robust local HTTP server wrapper for the TypeSwift project."
        echo ""
        set_color yellow; echo "USAGE:"
        set_color normal; echo "  typeswift [OPTIONS]"
        echo ""
        set_color yellow; echo "OPTIONS:"
        set_color normal; echo "  -h, --help      Show this help message and exit"
        echo "  -p, --port=NUM  Port to run the server on (default: 8000)"
        echo "  -d, --dir=PATH  Directory to serve (default: ~/Documents/tools/typeswift/)"
        echo "  -b, --bind=IP   Bind address (default: 127.0.0.1)"
        echo "  -n, --no-open   Do not automatically open the browser"
        return 0
    end

    # =========================================================================
    # 3. Default Configurations & Overrides
    # =========================================================================
    set -l port 8000
    set -l bind "127.0.0.1"
    set -l target_dir "~/Documents/tools/typeswift/"
    set -l open_browser 1

    if set -q _flag_port; set port $_flag_port; end
    if set -q _flag_bind; set bind $_flag_bind; end
    if set -q _flag_dir; set target_dir $_flag_dir; end
    if set -q _flag_no_open; set open_browser 0; end

    # =========================================================================
    # 4. Validations
    # =========================================================================
    if not string match -qr '^[0-9]+$' $port
        set_color red; echo "Error: Port must be a valid number (received: $port)."
        set_color normal; return 1
    end

    set -l expanded_dir (string replace "~" $HOME $target_dir)
    
    if not test -d "$expanded_dir"
        set_color red; echo "Error: Target directory does not exist."
        set_color normal; echo "Path: $expanded_dir"
        return 1
    end

    set -l py_cmd ""
    if command -v python3 >/dev/null
        set py_cmd python3
    else if command -v python >/dev/null
        set py_cmd python
    else
        set_color red; echo "Error: Python is not installed or not found in PATH."
        set_color normal; return 1
    end

    # =========================================================================
    # 5. Background Browser Launch (FIXED: Using native begin/end block)
    # =========================================================================
    set -l url "http://$bind:$port"
    
    if test $open_browser -eq 1
        # Native fish background block - bypasses quoting parser issues
        begin
            sleep 0.5
            if command -v open >/dev/null
                open "$url"
            else if command -v xdg-open >/dev/null
                xdg-open "$url"
            else if command -v start >/dev/null
                start "$url"
            end
        end >/dev/null 2>&1 &
    end

    # =========================================================================
    # 6. UI Output & Execution
    # =========================================================================
    echo ""
    set_color cyan; echo "🚀 Starting TypeSwift Dev Server..."
    set_color normal
    echo "📂 Directory: "(set_color green)$expanded_dir(set_color normal)
    echo "🌐 URL:       "(set_color blue; set_color -u)$url(set_color normal)
    echo "🐍 Runtime:   $py_cmd"
    echo ""
    set_color yellow; echo "Press Ctrl+C to stop the server."
    set_color normal; echo ""

    $py_cmd -m http.server $port --bind $bind --directory "$expanded_dir"
    
    # =========================================================================
    # 7. Graceful Cleanup
    # =========================================================================
    set -l exit_code $status
    echo ""
    if test $exit_code -eq 0 -o $exit_code -eq 130
        set_color green; echo "🛑 Server stopped gracefully."
    else
        set_color red; echo "⚠️ Server stopped with an unexpected exit code ($exit_code)."
    end
    set_color normal
end
