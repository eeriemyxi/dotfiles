# Introduction
My primary code editor as of Nov 18 2024. It is great. I will document all the
tricks I know here that I may forget due to infrequent use. I use Spacemacs
framework so a subset of the shortcuts listed here are from that framework.

Update (20 November 2024): I switched to my own config, ditching Spacemacs.

# General
- `M-x s d` see the diff of the current buffer and its corresponding file
- `M-q` wrap the content at the cursor
  - `M-x set-fill-column` set max length. 88 recommended
- `C-x h` select all content of the buffer
- `C-u M-!` insert output of a shell command into buffer
- `<leader>N` jump to a character using `avy-goto-char-timer`
- `<leader>n` jump to a character using `avy-goto-char-2`
- `<leader>f` open directory using `dired-jump`
- `<leader>a` search with `counsel-grep-or-swiper`
- `<leader>'` toggle shell with `shell-pop`
- `<leader>TAB` switch to last buffer with `mode-line-other-buffer`
- `C-<left>` move to the previous tab with `centaur-tabs-backward`
- `C-<right>` move to the next tab with `centaur-tabs-forward`
- `C-v` paste clipboard contents in normal/insert/visual modes
- `C-,` duplicate the current line in normal/insert modes
- `<leader>p` invoke `counsel-M-x`
- `<leader>y` open `post-init.el`
- `<leader>Y` reload init file
- `<leader>Q` quit Emacs
- `<leader>q` quit all buffers

### Shell
- `C-c-C-c` close the process
- `C-d` send EOF

### File Management
- `<leader>i` jump to a file with `counsel-file-jump`
- `<leader>I` search in the buffer with `swiper-isearch`

### Tabs and Zoom
- `<leader>t` switch tabs with `centaur-tabs-ace-jump`
- `<leader>z` adjust zoom with `hydra-zoom/body`

### Window Management
- `<leader>wn` move to the window below
- `<leader>we` move to the window above
- `<leader>wh` move to the right window
- `<leader>wi` move to the left window
- `<leader>wy` delete the current window

### Compilation
- `<leader>cn` go to the next error
- `<leader>ce` go to the previous error
- `<leader>cc` compile the project
- `<leader>cr` recompile the project
- `<leader>ck` kill compilation
- `<leader>cy` toggle compilation window
- `<leader>cu` switch to compilation buffer

### Buffers and Bookmarks
- `<leader>hp` kill the current buffer
- `<leader>ht` open a bookmark
- `<leader>hT` delete a bookmark
- `<leader>hf` open recent buffers or files
- `<leader>hm` switch to `*Messages*` buffer
- `<leader>hs` switch to `*scratch*` buffer

### Text Surround and Alignment
- `<leader>ds` add surrounding text in visual/normal modes
- `<leader>dc` change surrounding text
- `<leader>dd` delete surrounding text
- `<leader>ua` align text with regex
- `<leader>un` manage numbers with `hydra-numbers/body`

### Code Cleanup
- `<leader>uu` delete trailing whitespace
- `<leader>ut` manage TODOs with `hydra-todo/body`
- `<leader>uh` clear search highlights
- `<leader>uc` toggle comment region
- `<leader>us` substitute text in the selection
- `<leader>uS` query replace in the selection

## Save When Client Is Not Focused
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
