# My Emacs Config and Cheatsheet

## Installation

### macOS

The `railwaycat/emacsmacport` version is compatible with `spectacle` snapping
(some emacs distribution have an undesired space left around the borders after
snapping).

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

- `C`        Ctrl
- `M`        Meta (can be Alt or Cmd on macOS depending on setup)
- `s`        Super key (Windows Key or Cmd/Alt - see above)
- `S`        Shift

## Generally useful commands

- `C-h C-h`            display help
- `C-g`                cancel current action, quit minibuffer
- `<ESC> <ESC> <ESC>`  get out of all recursive editing levels
- `M-x`                run commands

## Help

- `C-h ?`       list all possible helps in this context
- `C-h c`       show oneline docs for a command (e.g. C-h c C-p)
- `C-h k`       show docs for a command key (e.g. C-h k C-p)
- `C-h f`       show docs for a command function (e.g. C-h f replace-string)
- `C-h v`       show docs for a variable
- `C-h i`       show manuals for installed packages

## Movement

- `C-v`         move one page forward
- `M-v`         move one page backward

- `C-f`         move one char forward
- `C-b`         move one char backward
- `M-f`         move one word forward
- `M-b`         move one word backward

- `C-n`         move to next line
- `C-p`         move to previous line

- `C-l`         move point to middle/top/back (cyccle) of window

- `M-<`         move to beginning of buffer
- `M->`         move to end of buffer

- `C-a`         move to beginning of line
- `C-e`         move to end of line
- `M-a`         move to beginning of sentence
- `M-e`         move to end of sentence

- `M-g M-g`     move to line number (interactive)

- `C-u C-<SPC>` move to previous mark (many operations leave marks in buffers)

## Numeric parameter

- `C-<num>`, `C-u <num>` set numeric parameter
  * alters behaviour of other commands, e.g. C-u 10 C-f moves 10 times forward
  * can be used to repeat text, e.g. C-u 8 * will output `********`

## Text manipulation

- `<DEL>`       delete (not kill) char before point
- `C-d`         delete (not kill) char after point

- `M-<DEL>`     kill word before point
- `M-d`         kill word after point

- `C-k`         kill text from point to end of line
- `M-k`         kill text from point to end of sentence

- `C-<SPC>`     grow or shrink region with point movement
- `C-w`         kill selected region
- `M-w`         kill text in region mark without removing (ctrl + c)

- `C-y`         yank last
- `M-y`         yank from kill ring (cycle)

- `C-/`         undo
- `C-S-/`       redo

## Search & Replace

- `C-s`         start i-search (incremental search)
- `C-r`         start backwards i-search
- `M-%`         start query replace
- `C-M-%`       start regexp query replace

## Window management

- `C-x 0`       close current window
- `C-x 1`       close all other windows
- `C-x 2`       split vertically
- `C-x 3`       split horizontally
- `C-x o`       focus next active window
- `C-M-v`       scroll text (C-v) in other window

## Frame management

- `C-x 5 2`     open a new frame (what other programs call a window)
- `C-x 5 0`     kill current frame

## File manipulation

- `C-x C-f`     find or create a file and open it in a new buffer
- `C-x s`       save files interactively
- `C-x C-s`     save file
- `C-x C-w`     save file as ...

## Buffers

- `C-x C-b`     list all open buffers
- `C-x b`       switch to a buffer
- `C-x k`       kill a buffer
- `C-x <right>` open next buffer
- `C-x <left>`  open previous buffer
- `C-x C-q`    toggle read-only for current buffer

## Major modes

### Dired

Dired is a directory explorer and editor.

In Dired:

- `(Dired) ?`   show help
- `(Dired) d`   mark file at point for deletion
- `(Dired) u`   unmark file at point for deletion
- `(Dired) x`   delete files marked for deletion
- `(Dired) f`   find/open file at point
- `(Dired) o`   find/open file at point in other window
- `(Dired) R`   move/rename file
- `(Dired) C`   copy file

A very powerful feature of Dired is, that like other buffers one can
toggle it's read-only status with `C-x C-q`.

## Minor modes

### Auto Fill

- `M-q`         re-fill current paragraph; useful for formatting prose or comments

## Extensions

### Straight and package management

When running the first time, straight.el seems to be ignoring the
lockfile and uses the newest versions instead. Running
`straight-thaw-versions` and restarting has the desired effect.

- `straight-freeze-versions` Save currently checked out versions to lockfile
- `straight-thaw-versions`   Check out versions saved in lockfile (but
                             does not apply them)
- `straight-fetch-all`       Fetch all git repos

### Projectile

- `s-p p`       Switch to project
- `s-p f`       Switch to file in current project
- `s-p a`       Add a project to projectile's index
- `s-p r`       Remove a project from projectile's index

### Swiper

- `C-s`         swiper search (replaces i-search)

### Counsel

- `M-x`         counsel powered M-x
- `C-x C-f`     counsel powered C-x C-f
- `C-c s g`     counsel git grep
- `C-c s r`     counsel ripgrep

### Crux

- `C-c d`       crux duplicate current line or region
- `s-k`         crux kill whole line
- `C-a`         crux move to beginning of line (smarter than classic `C-a`)

### Expand Region (very useful with Multiple Cursors below)

- `C-:`         Expand currently selected region based on syntax (mostly dwim)

### Multiple Cursors

- `C->`         Mark next like this (selection)
- `C-<`         Unmark previous like this (selection)
- `C-c l a`     Mark all like this (dwim)
- `C-c l l`     Spawn cursor in the beginning of each line in region

### Magit

- `s-m m`       magit status

In magit status:

- `(magit status) ?`   help
- `(magit status) d d` diff file or group (e.g. staged or unstaged files)
- `(magit status) s`   stage file at point
- `(magit status) u`   unstage file at point
- `(magit status) c c` commit
- `(magit status) P p` push to remote tracking branch
- `(magit status) b b` checkout branch (`git checkout`)
- `(magit status) b c` checkout new branch (`git checkout -b`)

In magit commit:

- `(magit commit) C-c C-c`     commit (with-editor-finish)

### Move text

- `M-p`         move line or region up
- `M-n`         move line or region down

### Rust mode

- `M-.`         jump to definition

### Tide

- `M-.` jump to definition
