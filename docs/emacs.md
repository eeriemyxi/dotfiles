# Introduction
My primary editor as of Mon Nov 18 2024. It is great. I will document tricks
here that I may forget due to infrequent use.

# General
- `M-x s d` see the diff of the current buffer and its corresponding file
- `M-q` wrap the content at the cursor
- `C-x h` select all content of the buffer
- `C-u M-!` insert stdout of a shell command into buffer
### Shell
- `C-c-C-c` close the process
- `C-d` send EOF, just as expected from normal terminal

## Installing Packages from Git
#### Example

```lisp
(orgqda :location (recipe :fetcher gitlab :repo "andersjohansson/orgqda"))
```

More information [here](https://github.com/syl20bnr/spacemacs/issues/12530).
