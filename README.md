# My Emacs Config and Cheatsheet

## Installation

### Linux

Nothing special here. Install from the OS's package manager and enjoy.

### Windows

Use the official binary distribution. Set up a shortcut for `bin\runemacs.exe`
and pin that. By default Emacs on Windows won't detect the `.emacs.d` config
directory anywhere. There are multiple ways to point Emacs to it, the easiest
being adding a `HOME` environment variable and Emacs will look for the config at
`%HOME\.emacs.d`.

You can enable native compilation of Emacs Lisp by having `libgccjit`. Get it
with MSYS2: `pacman -S mingw-w64-x86_64-libgccjit` and add it to PATH:
`C:\msys64\mingw64\bin`.

### macOS

Download and install Emacs for Mac https://emacsformacosx.com/

### External tools

Some commands rely on `git` and `rg` being installed and in PATH.

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

- `C-l`         move point to middle/top/back (cycle) of window

- `M-<`         move to beginning of buffer
- `M->`         move to end of buffer

- `C-a`         move to beginning of line
- `C-e`         move to end of line
- `M-a`         move to beginning of sentence
- `M-e`         move to end of sentence

- `M-g M-g`     move to line number (interactive)

- `C-u C-<SPC>` move to previous mark (many operations leave marks in buffers)

- `C-M-f`       forward-sexp
- `C-M-b`       backward-sexp

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

- `C-<SPC>`     start region selection, grow or shrink region with point movement
- `C-w`         kill selected region (ctrl + x)
- `M-w`         kill text in region mark without removing (ctrl + c)

- `C-y`         yank last
- `M-y`         yank from kill ring (cycle)

- `C-/`         undo

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
- `C-x 4 0`     kill current buffer and window
- `C-x o`       focus next active window
- `C-M-v`       scroll text (C-v) in other window

## Frame management

- `C-x 5 2`     open a new frame (what other programs call a window)
- `C-x 5 0`     kill current frame

## File manipulation

- `C-x C-f`     find or create-in-memory a file and open it in a new buffer
- `C-x s`       save all file visiting buffers interactively
- `C-x C-s`     save current file visiting buffer
- `C-x C-w`     save file as ...

## Buffers

- `C-x b`       switch to a buffer
- `C-x k`       kill a buffer
- `C-x C-q`     toggle read-only for current buffer

## Projects (project.el)

- `C-x p p`     switch to a project
- `C-x p f`     switch to a project file
- `C-x p b`     switch to a project buffer
- `C-x p r`     start project-wide regexp query replace

## Misc

- `M-q`         re-fill current paragraph; useful for formatting prose or comments
- `M-g g`       go to line

## Custom navigation and editing functions

- `M-n`         move to next completely blank line
- `M-p`         move to previous completely blank line
- `C-a`         toggle between beginning of line and first non-whitespace character
- `C-j`         join next line to this one

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

## Extensions

### Straight and package management

When running the first time, straight.el seems to be ignoring the
lockfile and uses the newest versions instead. Running
`straight-thaw-versions` and restarting has the desired effect.

- `straight-freeze-versions` Save currently checked out versions to lockfile
- `straight-thaw-versions`   Check out versions saved in lockfile (but
                             does not apply them)
- `straight-pull-all`        Pull and rebuild all packages. Use `straight-freeze-versions`
                             after to save the state

### Swiper

- `C-s`         swiper search (replaces i-search)

### Counsel

- `M-x`         counsel powered M-x
- `C-x C-f`     counsel-find-file
- `C-c s g`     counsel-git-grep
- `C-c s r`     counsel-rg

- `(counsel) C-M-j` use literal search string instead of completion (useful
                    for `counsel-find-file`); alternatively navigate to
                    search string

### Expand Region (very useful with Multiple Cursors below)

- `C-:`         Expand currently selected region based on syntax (mostly dwim)

### Multiple Cursors

- `C->`         Mark next like this (selection)
- `C-<`         Unmark previous like this (selection)
- `C-c m c`     Mark all like this (dwim)
- `C-c m l`     Spawn cursor in the beginning of each line in region

### Magit

- `C-c g g`       magit status

In magit status:

- `(magit status) ?`        help
- `(magit status) q`        close magit status window
- `(magit status) g`        refresh
- `(magit status) <tab>`    toggle section at point (diff)
- `(magit status) <return>` visit section at point
- `(magit status) d d`      diff file or group (e.g. staged or unstaged files)
- `(magit status) s`        stage file or hunk at point, or a selected line region
- `(magit status) u`        unstage file or hunk at point, or a selected line region
- `(magit status) k`        discard changes in file or hunk at point, or a selected line region
- `(magit status) c c`      commit
- `(magit status) c a`      amend previous commit
- `(magit status) m m`      start a merge
- `(magit status) r i`      start an interactive rebase
- `(magit status) P p`      push to push-remote branch
- `(magit status) P -f p`   push to push-remote branch using `--force-with-lease`
- `(magit status) F u`      pull from upstream
- `(magit status) f u`      fetch from upstream
- `(magit status) f -p u`   fetch from upstream with pruning dead branches
- `(magit status) b b`      checkout branch (`git checkout`)
- `(magit status) b c`      checkout new branch (`git checkout -b`)
- `(magit status) b s`      checkout new branch as spin-off, moving already commited changes

In magit commit:

- `(magit commit) C-c C-c`     commit (with-editor-finish)
- `(magit commit) C-c C-k`     cancel (with-editor-cancel)

### Move text

- `C-M-p`        move line or region up
- `C-M-n`        move line or region down

### Rust

- `rust-compile`     compiles current cargo project and opens *compilation* buffer in other window

In *compilation* buffer:

- `C-p`                navigate to previous line
- `C-n`                navigate to next line
- `M-p`                navigate to previous compile error or link
- `M-n`                navigate to next compile error or link
- `C-o` or `<return>`  go to compile error or link in other window
