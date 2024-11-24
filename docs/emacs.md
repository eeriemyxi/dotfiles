# Introduction
My primary code editor as of Nov 18 2024. It is great. I will document all the
tricks I know here that I may forget due to infrequent use. I use Spacemacs
framework so a subset of the shortcuts listed here are from that framework.

- Update (20 November 2024): I switched to my own config, ditching Spacemacs.
- Update (24 November 2024): I switched from [Evil
Mode](https://github.com/emacs-evil/evil "Evil Modal Editing") to [Meow
Mode](https://github.com/meow-edit/meow "Meow Modal Editing").

# General
- `M-x s d` see the diff of the current buffer and its corresponding file
- `M-q` wrap the content at the cursor
  - `M-x set-fill-column` set max length. 88 recommended
- `C-x h` select all content of the buffer
- `C-u M-!` insert output of a shell command into buffer

# Editing and Surrounding
- `C-c C-SPC C-s` add or remove surrounding text
- `C-c C-SPC T` apply title case to a region
- `C-c C-SPC C-u u` remove trailing whitespace
- `C-c C-SPC C-u a` align by regular expression
- `C-c C-SPC C-u s` perform a find-and-replace
- `C-c C-SPC C-u S` perform a regex find-and-replace

# Navigation
- `C-c C-SPC C-n t` jump to a character pair using `avy`
- `C-c C-SPC C-n T` jump with a character timer using `avy`
- `C-c C-SPC C-n I` search text using `swiper`
- `C-c C-SPC C-n a` search or grep using `counsel`
- `C-c C-SPC C-n i` jump to files using `counsel`
- `C-c C-SPC C-n n` manage bookmarks using `counsel`
- `C-c C-SPC C-n f` open recent files or buffers using `counsel`

# File and Buffer Management
- `C-c C-SPC f` open file in Dired
- `C-c C-SPC y` edit `post-init.el` file
- `C-c C-SPC Y` reload configuration
- `C-c C-SPC q` quit window
- `C-c C-SPC Q` exit Emacs
- `C-c C-SPC TAB` switch to the previous buffer
- `C-c C-SPC C-k p` close current buffer
- `C-c C-SPC C-k P` kill another buffer
- `C-c C-SPC C-k T` delete a bookmark
- `C-c C-SPC C-k m` open messages buffer
- `C-c C-SPC C-k s` switch to scratch buffer

# Window Management
- `C-c C-SPC C-w n` move window focus downward
- `C-c C-SPC C-w e` move window focus upward
- `C-c C-SPC C-w h` move window focus rightward
- `C-c C-SPC C-w i` move window focus leftward
- `C-c C-SPC C-w y` delete window

# Compilation and Error Navigation
- `C-c C-SPC C-c y` toggle compilation window
- `C-c C-SPC C-c s` switch to compilation buffer
- `C-c C-SPC C-c c` compile
- `C-c C-SPC C-c n` jump to the next error
- `C-c C-SPC C-c e` jump to the previous error
- `C-c C-SPC C-c k` kill compilation

# Language Server Protocol (LSP)
- `C-c C-SPC C-l l` start the `eglot` LSP client
- `C-c C-SPC C-l s` stop `eglot`
- `C-c C-SPC C-l d` show documentation with `eldoc`
- `C-c C-SPC C-l r` rename using `eglot`
- `C-c C-SPC C-l R` reconnect `eglot`
- `C-c C-SPC C-l f` format buffer with `eglot`
- `C-c C-SPC C-l i` organize imports with `eglot`

# Custom and Miscellaneous Commands
- `C-c C-SPC t` use custom paging commands
- `C-c C-SPC C-u n` open number editing Hydra
- `C-c C-SPC C-u t` open TODO management Hydra
- `C-c C-SPC C-u h` clear search highlights
