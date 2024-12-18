# Introduction
My primary code editor as of Nov 18 2024. It is great. I will document all the
tricks I know here that I may forget due to infrequent use. I use Spacemacs
framework so a subset of the shortcuts listed here are from that framework.

- Update (20 November 2024): I switched to my own config, ditching Spacemacs.
- Update (24 November 2024): I switched from [Evil
Mode](https://github.com/emacs-evil/evil "Evil Modal Editing") to [Meow
Mode](https://github.com/meow-edit/meow "Meow Modal Editing").
- Update (18 November 2024): I switched from [Meow Mode](https://github.com/meow-edit/meow) to [Boon Mode](https://github.com/jyp/boon).

# General
- `M-x s d` see the diff of the current buffer and its corresponding file
- `M-q` wrap the content at the cursor
  - `M-x set-fill-column` set max length. 88 recommended
- `C-x h` select all content of the buffer
- `C-u M-!` insert output of a shell command into buffer

# Editing and Surrounding
- `M-' s` add or remove surrounding text
- `M-' T` apply title case to a region
- `M-' u u` remove trailing whitespace
- `M-' u a` align by regular expression
- `M-' u s` perform a find-and-replace
- `M-' u S` perform a regex find-and-replace

# Navigation
- `M-' n t` jump to a character pair using `avy`
- `M-' n T` jump with a character timer using `avy`
- `M-' n I` search text using `swiper`
- `M-' n a` search or grep using `counsel`
- `M-' n i` jump to files using `counsel`
- `M-' n n` manage bookmarks using `counsel`
- `M-' n f` open recent files or buffers using `counsel`

# File and Buffer Management
- `M-' f` open file in Dired
- `M-' y` edit `post-init.el` file
- `M-' Y` reload configuration
- `M-' q` quit window
- `M-' Q` exit Emacs
- `M-' TAB` switch to the previous buffer
- `M-' k p` close current buffer
- `M-' k P` kill another buffer
- `M-' k T` delete a bookmark
- `M-' k m` open messages buffer
- `M-' k s` switch to scratch buffer

# Window Management
- `M-' w n` move window focus downward
- `M-' w e` move window focus upward
- `M-' w h` move window focus rightward
- `M-' w i` move window focus leftward
- `M-' w y` delete window

# Compilation and Error Navigation
- `M-' c y` toggle compilation window
- `M-' c s` switch to compilation buffer
- `M-' c c` compile
- `M-' c n` jump to the next error
- `M-' c e` jump to the previous error
- `M-' c k` kill compilation

# Language Server Protocol (LSP)
- `M-' l l` start the `eglot` LSP client
- `M-' l s` stop `eglot`
- `M-' l d` show documentation with `eldoc`
- `M-' l r` rename using `eglot`
- `M-' l R` reconnect `eglot`
- `M-' l f` format buffer with `eglot`
- `M-' l i` organize imports with `eglot`

# Custom and Miscellaneous Commands
- `M-' h t` use custom paging commands
- `M-' h n` open number editing Hydra
- `M-' h T` open TODO management Hydra
- `M-' h h` clear search highlights
