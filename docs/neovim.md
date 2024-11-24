# Introduction

Neovim is one of those apps I actively add new features to then strugle to
remember them due to my lack of muscle memory for it. But when I do want to use
them I forget what the keymap is. I'd then have to go through my config files to
find out the shortcut, which is inconvenient.

Here I am going to document everything I should be needing here so that I can
read them when I get the intuition that I forgot something useful.

# General

- `vU` uppercase selection.
- `vl` lowercase selection.
- `.` redo last action.
- `gl` and `gh` to go to start and end of the line.
- `]<space>` and `]<space>` to add empty line in the next or previous line.
- `vuw` and `vuW` to select current word.
- `vup` to select current paragraph.
- `vu[some symbol like "]` to select content within those symbols under context
  of the cursor.
- `N` and `E` to scroll down and up.
- `:new` to open a file in another window.
- `<space>` + `[hnei]` to move cursor to another window.
- `<space>` + `[HNEI]` to move the current window.
- `<space>f` open file switcher.
  - `<C-x>` to open the file as a split.
  - `<C-v>` to open the file as a vertical split.
  - `<C-t>` to open the file in a new tab.
- `<space>F` open buffer switcher.
- `<space>d` diagnostics.
- `k` go to the next match (from using `/`).
- `C-o` go to previous match (from using `/`).
- `;s` hide search highlights.
- `;q` toggle undo branch tree.
- `;p` find text in the source tree.
- `;r` LSP identifier rename (hover over the variable with the cursor then use
  it).
- `;w` LSP definitions.
- `;u` toggle LSP diagnostics.
- `ZZ` quit the editor.
- `Zz` to force quit the editor.

### Spell Checking

- `;d` to toggle spell checking.
- `]s` and `[s` to go to next or previous spelling error.
- `z=` to find suggestions for the current error under cursor.

### Macros

- `q` to record macros, e.g., `qq` will record it in `q` registry.
- `@` to trigger a macro, e.g., `@q` will trigger the `q` registry.

**Deleting Stuff**

- Visual mode
  - `x` to delete **without** copy.
  - `d` to delete **with** copy.
- Normal mode
  - `x` to delete **without** copy.
  - `dd` to delete current line **without** copy.

### Substitution

This editor's text replacement feature is accessed via `:substitute` or `:s`

- Selecting lines then doing `:` will insert something into the command line.
  Those characters help the command understand that you want to execute it in
  only the selected area.
- They use regular expressions to match stuff.
- `:%s/hello/hwello` this will replace first "hello" in every line of the file.
  `%` selects the entire file.
- `:%s/hello/hwello/g` this will replace all "hello" of the file. `%` selects
  the entire file. `/g` will apply it to every match rather the first one in a
  line.
- `:%s/^/something` this will add "something" to the start of every line.
- `:%s/$/something` this will add "something" to the end of every line.

# Plugins

### Tab Line

Filename: `tab-line.lua`

This is different from the status line, that is one at the bottom and this one
is at the top.

- `<space>th` move to the previous tab
- `<space>ti` move to the next tab
- `Alt + <0-9>` move to a specific tab.
- `<space>tc` close the tab.
- `<space>t<space>` select a tab.

### Surrounder

Filename: `surrounder.lua`

This plugin helps surround/strip a character from visual selection.

- `<space>kk` then a symbol. To append.
- `<space>kr` then a symbol. To replace.
- `<space>kd` to delete.

### Bufferized File Explorer

Filename: `bufferized-file-explorer.lua`

- `<space>tr` to trigger.
  - `g.` to toggle hidden files.

### Markdown Previews

Filename: `markdown-previews.md`

View markdown files in a web browser as rendered.

- `:MaT` is an alias to toggle the renderer.

### To Title

Filename: `text-enhancers.md`

Convert to title-case.

- `gt` to convert visual selection.
- `gtt` to convert the whole line.
- `gt2w` to convert the next two words.
