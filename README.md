# Emacs Cheatsheet

## Installation

### macOS

The `railwaycat/emacsmacport` version is compatible with `spectacle` snapping
`https://github.com/railwaycat/homebrew-emacsmacport`

Install it from `homebrew`:

``` shell
$ brew tap railwaycat/emacsmacport
$ brew install emacs-mac
```

There is also an `emacs` formula and `d12frosted/emacs-plus` that might work.

``` shell
$ brew install emacs
```

or

``` shell
$ brew tap d12frosted/emacs-plus
$ brew install emacs-plus
```

### Linux

Coming soon.

## Modifier Keys

`C`        Ctrl
`M`        Meta (can be Alt or Cmd on macOS depending on setup, usually Alt)
`s`        Super key (Windows Key or Cmd/Alt - see above)
`S`        Shift

## Generally useful commands

`C-h C-h`            display help
`C-g`                cancel current action, quit minibuffer
`<ESC> <ESC> <ESC>`  get out of all recursive editing levels
`M-x`                run commands (Ctrl/Cmd + P on sublime/atom/vs-code)

## Help

`C-h ?`       list all possible helps in this context
`C-h c`       show oneline docs for a command (e.g. C-h c C-p)
`C-h k`       show docs for a command key (e.g. C-h k C-p)
`C-h f`       show docs for a command function (e.g. C-h f replace-string)
`C-h v`       show docs for a variable
`C-h a`       show docs for a keyword?
`C-h i`       show manuals for installed packages

## Useful packages

### Used

ivy               completions list
swiper            replaces i-search (built on ivy)
counsel           completions everywhere (built on ivy)

neotree           atom-like file explorer

magit             git interface
projectile        project management (cmd+p)

doom-themes       various color themes

ace-window        better other-window
avy               goto char/word using char-based decision tree
crux              collection of misc useful extensions (see below)
diff-hl           highlights git diffs
editorconfig      editorconfig support
epl               emacs perl; not sure how exactly, but used in updater
expand-region     expand selected region syntactically
flycheck          syntax checking for languages
git-timemachine   convenient git history browsing for current file
move-text         move line/region up/down
smartparens       automatically close parens, slurping, wrapping, unwrapping
undo-tree         nonlinear (tree) undo/redo
which-key         expand minibuffer after delay to show bound keys/commands
zop-to-char       visual zap-to-char (do op on region between point and char)

### To Learn

diff view with magit

#### To Try

multiple-cursors  multiple cursors
company           code completions

## Movement

`C-v`         move one page forward
`M-v`         move one page backward

`C-f`         move one char forward
`C-b`         move one char backward
`M-f`         move one word forward
`M-b`         move one word backward

`C-n`         move to next line
`C-p`         move to previous line

`C-l`         move point to middle/top/back (cyccle) of window

`M-<`         move to beginning of buffer
`M->`         move to end of buffer

`C-a`         move to beginning of line
`C-e`         move to end of line
`M-a`         move to beginning of sentence
`M-e`         move to end of sentence

## Numeric parameter

`C-<num>`, `C-u <num>`  set numeric parameter
  * alters behaviour of other commands, e.g. C-u 10 C-f moves 10 times forward
  * can be used to repeat text, e.g. C-u 8 * will output `********`

## Text manipulation

`<DEL>`       delete (not kill) char before point
`C-d`         delete (not kill) char after point

`M-<DEL>`     kill word before point
`M-d`         kill word after point

`C-k`         kill text from point to end of line
`M-k`         kill text from point to end of sentence

`C-<SPC>`     set region mark, grow or shrink with point movement
`C-w`         kill selected region
`M-w`         kill text in region mark without removing (ctrl + c)

`C-y`         yank last
`M-y`         yank from kill ring

`C-/`, `C-_`  undo last command

## Search

`C-s`         start incremental search
`C-r`         start backwards incremental search

## Window management

`C-x 0`       close current window
`C-x 1`       close all other windows
`C-x 2`       split vertically
`C-x 3`       split horizontally
`C-x o`       focus next active window
`C-M-v`       scroll text (C-v) in other window

## Frame management

`C-x 5 2`     open a new frame (what other programs call a window)
`C-x 5 0`     kill current frame

## File manipulation

`C-x C-f`     find or create a file and open it in a new buffer
`C-x s`       save files interactively
`C-x C-s`     save file
`C-x C-w`     save file as ...

## Buffers

`C-x C-b`     list all open buffers
`C-x b`       switch to a buffer
`C-x k`       kill a buffer
`C-x <right>` open next buffer
`C-x <left>`  open previous buffer

## eXtended commands and Names eXtended commands

`C-x`         show extended command menu
`M-x`         show named extended command menu

`M-x replace-string`      replaces occurences of first string with second string after point
`M-x recover-this-file`   restore file contents from autosave file (#filename.ext)

## Minor modes

### Auto Fill

`M-q`         re-fill current paragraph; useful for formatting prose or comments

## Extensions

### Avy

`s-.`         avy go to word or subword (char-based decision tree)

### Crux

`C-c d`       crux duplicate current line or region
`s k`         crux kill whole line
`C-a`         crux move to beginning of line (smarter than classic `C-a`)

### Magit

`C-x g`       magit status

#### Magit status

`s`           stage
`u`           unstage
`c c`         commit

#### Magit commit

`C-c C-c`     commit

### Move text

`s-M <up>`    move line or region up
`s-M <down>`  move line or region down

### Rust mode

`M-.`         jump to definition

### Swiper

`C-s`         swiper search (replaces i-search)

### Tide

`M-.`         jump to definition
