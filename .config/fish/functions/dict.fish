function dict
    sdcv -n $argv | pandoc -f html -t commonmark --wrap=none | sed -E 's/^#{3,6} /# /' | glow -
end
