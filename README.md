# Emacs Cheatsheet

## Installation

### macOS

The `railwaycat/emacsmacport` version is compatible with `spectacle` snapping
(there is undesired space left around the borders after snapping).

Install it from `homebrew`:

``` shell
$ brew tap railwaycat/emacsmacport
$ brew install emacs-mac
```

Alternatively, the `d12frosted/emacs-plus` is recommended by `spacemacs`.
Previously it had issues with `spectacle`snapping, but those may have been
resolved meanwhile, or could be resolved by using the appropriate `brew install`
flags.

``` shell
$ brew tap d12frosted/emacs-plus
$ brew install emacs-plus
```

There is also an official `emacs` formula:

``` shell
$ brew install emacs
```

## Modifier Keys

`C`        Ctrl
`M`        Meta (can be Alt or Cmd on macOS depending on setup)
`s`        Super key (Windows Key or Cmd/Alt - see above)
`S`        Shift

## Generally useful commands

`C-h C-h`            display help
`C-g`                cancel current action, quit minibuffer
`<ESC> <ESC> <ESC>`  get out of all recursive editing levels
`M-x`                run commands

## Help

`C-h ?`       list all possible helps in this context
`C-h c`       show oneline docs for a command (e.g. C-h c C-p)
`C-h k`       show docs for a command key (e.g. C-h k C-p)
`C-h f`       show docs for a command function (e.g. C-h f replace-string)
`C-h v`       show docs for a variable
`C-h i`       show manuals for installed packages

## Useful packages

ivy               completions list
swiper            replaces i-search (built on ivy)
counsel           ivy completions in various places, e.g. M-x or project grep

company           various autocomples, e.g. function name, variables, text

magit             git interface
projectile        project management (cmd+p, search within project)

doom-themes       various color themes

ace-window        better other-window
avy               goto word using char-based decision tree
crux              collection of misc useful commands (see below)
diff-hl           highlights git diffs
editorconfig      editorconfig support
expand-region     expand selected region syntactically
flycheck          syntax checking for languages
git-timemachine   convenient git history browsing for current file
move-text         move line/region up/down
multiple-cursors  duplicate cursor and edit multiple places at once
smartparens       automatically close parens, slurping, wrapping, unwrapping
which-key         expand minibuffer after delay to show bound keys/commands

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

`M-g M-g`     move to line number (interactive)

`C-u C-<SPC>` move to previous mark (many operations leave marks in buffers)

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

`C-<SPC>`     grow or shrink region with point movement
`C-w`         kill selected region
`M-w`         kill text in region mark without removing (ctrl + c)

`C-y`         yank last
`M-y`         yank from kill ring (cycle)

`C-/`         undo
`C-S-/`       redo

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

## Minor modes

### Auto Fill

`M-q`         re-fill current paragraph; useful for formatting prose or comments

## Extensions

### Projectile

`s-p p`       Switch to project
`s-p f`       Switch to file in current project
`s-p D`       Open Dired in project root
`s-p d`       Find a directory in project and open Dired in it
`s-p r`       Project search and replace
`s-p s g`     Project grep
`s-p s r`     Project ripgrep
`projectile-add-known-project` Add a project to projectile's index

### Swiper

`C-s`         swiper search (replaces i-search)

### Counsel

`M-x`         counsel powered M-x
`C-x C-f`     counsel powered C-x C-f
`C-c s g`     counsel git grep
`C-c s r`     counsel ripgrep

### Avy

`s-.`         avy go to word or subword (char-based decision tree)

### Crux

`C-c d`       crux duplicate current line or region
`s-k`         crux kill whole line
`C-a`         crux move to beginning of line (smarter than classic `C-a`)

### Multiple Cursors

`C->`         Mark next like this (selection)
`C-<`         Unmark previous like this (selection)
`C-c C->`     Mark all like this (selection)
`C-c m c`     Spawn cursor in the beginning of each line in region

### Magit

`s-m m`       magit status

In magit status:

`(magit status) ?`   help
`(magit status) d d` diff file or group (e.g. staged or unstaged files)
`(magit status) s`   stage file at point
`(magit status) u`   unstage file at point
`(magit status) c c` commit
`(magit status) P p` push to remote tracking branch

In magit commit:

`(magit commit) C-c C-c`     commit (with-editor-finish)

### Move text

`M-p`         move line or region up
`M-n`         move line or region down

### Rust mode

`M-.`         jump to definition

### Tide

`M-.`         jump to definition
