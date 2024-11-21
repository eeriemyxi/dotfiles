# Introduction
My primary code editor as of Nov 18 2024. It is great. I will document all the
tricks I know here that I may forget due to infrequent use. I use Spacemacs
framework so a subset of the shortcuts listed here are from that framework.

# General
- `M-x s d` see the diff of the current buffer and its corresponding file
- `M-q` wrap the content at the cursor
  - `M-x set-fill-column` set max length. 88 recommended
- `C-x h` select all content of the buffer
- `C-u M-!` insert output of a shell command into buffer
- `SPC t F` automatic word wrapping.
### Shell
- `C-c-C-c` close the process
- `C-d` send EOF
### Save When Client Is Not Focused
```lisp
(add-hook 'focus-out-hook (lambda ()
  (save-some-buffers t)))
```

## Installing Packages from Git
#### Example

```lisp
(orgqda :location (recipe :fetcher gitlab :repo "andersjohansson/orgqda"))
```

More information [here](https://github.com/syl20bnr/spacemacs/issues/12530).
