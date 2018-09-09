(message "Emacs is powering up...")

;; BOOTSTRAP

(message "Init phase: bootstrap")

(when (version< emacs-version "25.3")
  (error
   "This config requires GNU Emacs 25.3 or newer, but you're running %s"
   emacs-version))

;; Always load newest byte code
(setq load-prefer-newer t)

;; Define directories

(defvar ya/dir-root (file-name-directory load-file-name)
  "The root dir of this Emacs distribution.")

(defvar ya/dir-savefile (expand-file-name "savefile" ya/dir-root)
  "This directory stores all automatically generated save/history files.")

(unless (file-exists-p ya/dir-savefile)
  (make-directory ya/dir-savefile))

;; Increase GC threshold to 50MB and large file limit to 100MB

;; TODO: increase only for init
(setq gc-cons-threshold 50000000)
(setq large-file-warning-threshold 100000000)

;; PACKAGES

(message "Init phase: packages")

(require 'cl)
(require 'package)

;; Add MELPA to gain access to more packages
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Load pinned packages
(let ((ya/file-package-lock (expand-file-name "package-lock.el" ya/dir-root)))
  (if (file-exists-p ya/file-package-lock)
      (load ya/file-package-lock)))

;; Set package-user-dir relative to this config and initialize package manger
(setq package-user-dir (expand-file-name "elpa" ya/dir-root))
(package-initialize)

(defvar ya/packages
  ;; GENERAL PACKAGES
  '(ace-window
    avy
    crux
    diff-hl
    editorconfig
    expand-region
    flycheck
    git-timemachine
    projectile
    magit
    multiple-cursors
    move-text
    neotree
    smartparens
    undo-tree
    which-key
    doom-themes
    zop-to-char

    ;; Ivy
    ivy
    swiper
    counsel

    ;; Company
    company

    ;; LANGUAGE SPECIFIC PACKAGES

    ;; Lisp (shared for all lisps)
    rainbow-delimiters

    ;; Emacs Lisp
    elisp-slime-nav
    rainbow-mode

    ;; JSON
    json-mode

    ;; Web (HTML + CSS + JS)
    web-mode

    ;; JavaScript
    js2-mode

    ;; TypeScript
    tide

    ;; Rust
    rust-mode
    racer
    flycheck-rust
    cargo

    ;; Python
    ;; TODO

    ;; Haskell
    haskell-mode)
  "A list of packages to ensure are installed at launch.")

(defun ya/require-package (package)
  "Install PACKAGE unless already installed."
  (unless (memq package ya/packages)
    (add-to-list 'ya/packages package))
  (unless (package-installed-p package)
    (package-install package)))

(defun ya/require-packages (packages)
  "Ensure PACKAGES are installed, missing packages are installed automatically."
  (mapc #'ya/require-package packages))

;; Install all packages now
(unless (every #'package-installed-p ya/packages)
  ;; check for new packages (package versions)
  (message "%s" "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (ya/require-packages ya/packages))

;; UI

(message "Init phase: ui")

;; Reclaim some screen real-estate
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(menu-bar-mode -1)

(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; Disable cursor blinking
(blink-cursor-mode -1)

;; Disable the annoying sounds
(setq ring-bell-function 'ignore)

;; Disable startup screen
(setq inhibit-startup-screen t)

;; Add nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; Show line and column numbers, and file size indication
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; Enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; Add more useful frame title, Show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '("" invocation-name ""
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; Load theme
(load-theme 'doom-one t)
(setq
 doom-themes-enable-bold t
 doom-themes-enable-italic t)

;; Show available keybindings after you start typing
(require 'which-key)
(which-key-mode +1)

;; CORE

(message "Init phase: core")

(require 'thingatpt)
(require 'cl-lib)

(defun ya/recompile-init ()
  "Byte-compile all your dotfiles again."
  (interactive)
  (byte-recompile-directory ya/dir-root 0))

(defun ya/eval-after-init (form)
  "Add `(lambda () FORM)' to `after-init-hook'.
If Emacs has already finished initialization, also eval FORM immediately."
  (let ((func (list 'lambda nil form)))
    (add-hook 'after-init-hook func)
    (when after-init-time
      (eval form))))

;; MODE

(message "Init phase: mode")

(require 'easymenu)
(require 'crux)

;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key map (kbd "C-a") 'crux-move-beginning-of-line)

(global-set-key map [(shift return)] 'crux-smart-open-line)
(global-set-key map (kbd "M-o") 'crux-smart-open-line)
(global-set-key map [(control shift return)] 'crux-smart-open-line-above)
(global-set-key map (kbd "s-o") 'crux-smart-open-line-above)
(global-set-key map (kbd "C-c d") 'crux-duplicate-current-line-or-region)
(global-set-key map (kbd "C-c M-d") 'crux-duplicate-and-comment-current-line-or-region)
(global-set-key map (kbd "s-j") 'crux-top-join-line)
(global-set-key map (kbd "s-k") 'crux-kill-whole-line)

(global-set-key map (kbd "C-c f") 'crux-recentf-ido-find-file)
(global-set-key map (kbd "s-r") 'crux-recentf-ido-find-file)
(global-set-key map (kbd "C-c r") 'crux-rename-buffer-and-file)
(global-set-key map (kbd "C-c k") 'crux-kill-other-buffers)
(global-set-key map (kbd "C-c I") 'crux-find-user-init-file)
(global-set-key map (kbd "C-c S") 'crux-find-shell-init-file)

(global-set-key map [(control shift up)]  'move-text-up)
(global-set-key map [(control shift down)]  'move-text-down)
(global-set-key map [(meta shift up)]  'move-text-up)
(global-set-key map [(meta shift down)]  'move-text-down)

(global-set-key map (kbd "s-p") 'projectile-command-map)
(global-set-key map (kbd "C-c p") 'projectile-command-map)

(global-set-key map (kbd "s-m m") 'magit-status)
(global-set-key map (kbd "s-m l") 'magit-log)
(global-set-key map (kbd "s-m f") 'magit-log-buffer-file)
(global-set-key map (kbd "s-m b") 'magit-blame)

;; EDITOR

(message "Init phase: editor")

;; Death to tabs! However, tabs historically indent to the next
;; 8-character offset; specifying anything else will cause *mass*
;; confusion, as it will change the appearance of every existing file.
;; In some cases (python), even worse -- it will change the semantics.
;;
;; Emacs modes typically provide a standard means to change the
;; indentation width -- eg. c-basic-offset: use that to adjust your
;; personal indentation width, while maintaining the style (and
;; meaning) of any files you load.
(setq-default indent-tabs-mode nil)   ;; don't use tabs to indent
(setq-default tab-width 8)            ;; but maintain correct appearance

;; Newline at end of file
(setq require-final-newline t)

;; Delete the selection with a keypress
(delete-selection-mode t)

;; Store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Autosave the undo-tree history
(setq undo-tree-history-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq undo-tree-auto-save-history t)

;; Revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;; Hippie expand is dabbrev expand on steroids
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))

;; Smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

;; Smart pairing for all
(require 'smartparens-config)
(setq sp-base-key-bindings 'paredit)
(setq sp-autoskip-closing-pair 'always)
(setq sp-hybrid-kill-entire-symbol nil)
(sp-use-paredit-bindings)

(show-smartparens-global-mode +1)

;; Disable annoying blink-matching-paren
(setq blink-matching-paren nil)

;; Meaningful names for buffers with the same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)    ; Rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; Don't muck with special buffers

;; saveplace remembers your location in a file when saving files
(setq save-place-file (expand-file-name "saveplace" prelude-savefile-dir))
;; activate it for all buffers
(save-place-mode 1)

;; savehist keeps track of some history
(require 'savehist)
(setq savehist-additional-variables
      ;; search entries
      '(search-ring regexp-search-ring)
      ;; save every minute
      savehist-autosave-interval 60
      ;; keep the home clean
      savehist-file (expand-file-name "savehist" prelude-savefile-dir))
(savehist-mode +1)

;; save recent files
(require 'recentf)
(setq recentf-save-file (expand-file-name "recentf" prelude-savefile-dir)
      recentf-max-saved-items 500
      recentf-max-menu-items 15
      ;; disable recentf-cleanup on Emacs start, because it can cause
      ;; problems with remote files
      recentf-auto-cleanup 'never)

(defun prelude-recentf-exclude-p (file)
  "A predicate to decide whether to exclude FILE from recentf."
  (let ((file-dir (file-truename (file-name-directory file))))
    (cl-some (lambda (dir)
               (string-prefix-p dir file-dir))
             (mapcar 'file-truename (list prelude-savefile-dir package-user-dir)))))

(add-to-list 'recentf-exclude 'prelude-recentf-exclude-p)

(recentf-mode +1)

;; use shift + arrow keys to switch between visible buffers
(require 'windmove)
(windmove-default-keybindings)

;; automatically save buffers associated with files on buffer switch
;; and on windows switch
(defun prelude-auto-save-command ()
  "Save the current buffer if `prelude-auto-save' is not nil."
  (when (and prelude-auto-save
             buffer-file-name
             (buffer-modified-p (current-buffer))
             (file-writable-p buffer-file-name))
    (save-buffer)))

(defmacro advise-commands (advice-name commands class &rest body)
  "Apply advice named ADVICE-NAME to multiple COMMANDS.

The body of the advice is in BODY."
  `(progn
     ,@(mapcar (lambda (command)
                 `(defadvice ,command (,class ,(intern (concat (symbol-name command) "-" advice-name)) activate)
                    ,@body))
               commands)))

;; advise all window switching functions
(advise-commands "auto-save"
                 (switch-to-buffer other-window windmove-up windmove-down windmove-left windmove-right)
                 before
                 (prelude-auto-save-command))

(add-hook 'mouse-leave-buffer-hook 'prelude-auto-save-command)

(add-hook 'focus-out-hook 'prelude-auto-save-command)

(defadvice set-buffer-major-mode (after set-major-mode activate compile)
  "Set buffer major mode according to `auto-mode-alist'."
  (let* ((name (buffer-name buffer))
         (mode (assoc-default name auto-mode-alist 'string-match)))
    (when (and mode (consp mode))
      (setq mode (car mode)))
    (with-current-buffer buffer (if mode (funcall mode)))))

;; highlight the current line
(global-hl-line-mode +1)

;; note - this should be after volatile-highlights is required
;; add the ability to cut the current line, without marking it
(require 'rect)
(crux-with-region-or-line kill-region)

;; tramp, for sudo access
(require 'tramp)
;; keep in mind known issues with zsh - see emacs wiki
(setq tramp-default-method "ssh")

;; flyspell-mode does spell-checking on the fly as you type
(require 'flyspell)
(setq ispell-program-name "aspell" ; use aspell instead of ispell
      ispell-extra-args '("--sug-mode=ultra"))

(defun prelude-enable-flyspell ()
  "Enable command `flyspell-mode' if `prelude-flyspell' is not nil."
  (when (and prelude-flyspell (executable-find ispell-program-name))
    (flyspell-mode +1)))

(defun prelude-cleanup-maybe ()
  "Invoke `whitespace-cleanup' if `prelude-clean-whitespace-on-save' is not nil."
  (when prelude-clean-whitespace-on-save
    (whitespace-cleanup)))

(defun prelude-enable-whitespace ()
  "Enable `whitespace-mode' if `prelude-whitespace' is not nil."
  (when prelude-whitespace
    ;; keep the whitespace decent all the time (in this buffer)
    (add-hook 'before-save-hook 'prelude-cleanup-maybe nil t)
    (whitespace-mode +1)))

(add-hook 'text-mode-hook 'prelude-enable-flyspell)
(add-hook 'text-mode-hook 'prelude-enable-whitespace)

;; enable narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;; enabled change region case commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; enable erase-buffer command
(put 'erase-buffer 'disabled nil)

(require 'expand-region)

;; bookmarks
(require 'bookmark)
(setq bookmark-default-file (expand-file-name "bookmarks" ya/dir-savefile)
      bookmark-save-flag 1)

;; projectile is a project management mode
(require 'projectile)
(setq projectile-cache-file (expand-file-name  "projectile.cache" ya/dir-savefile))
(projectile-mode t)

;; avy allows us to effectively navigate to visible things
(require 'avy)
(setq avy-background t)
(setq avy-style 'at-full)

;; multiple cursors allows us to efficiently find/replace or edit multiple lines
(require 'multiple-cursors)
(global-set-key (kbd "C-c m c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)

;; dired - reuse current buffer by pressing 'a'
(put 'dired-find-alternate-file 'disabled nil)

;; Always delete and copy recursively
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)

;; If there is a dired buffer displayed in the next window, use its
;; current subdir, instead of the current subdir of this dired buffer
(setq dired-dwim-target t)

;; Enable some really cool extensions like C-x C-j(dired-jump)
(require 'dired-x)

;; ediff - don't start another frame
(require 'ediff)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Clean up obsolete buffers automatically
(require 'midnight)

(defadvice exchange-point-and-mark (before deactivate-mark activate compile)
  "When called with no active region, do not activate mark."
  (interactive
   (list (not (region-active-p)))))

(require 'tabify)
(defmacro with-region-or-buffer (func)
  "When called with no active region, call FUNC on current buffer."
  `(defadvice ,func (before with-region-or-buffer activate compile)
     (interactive
      (if mark-active
          (list (region-beginning) (region-end))
        (list (point-min) (point-max))))))

(with-region-or-buffer indent-region)
(with-region-or-buffer untabify)

;; abbrev config
(add-hook 'text-mode-hook 'abbrev-mode)

;; Whitespace-mode config
(require 'whitespace)
(setq whitespace-line-column 80)
(setq whitespace-style '(face tabs empty trailing lines-tail))

;; Have saner regex syntax
(require 're-builder)
(setq reb-re-syntax 'string)

(require 'eshell)
(setq eshell-directory-name (expand-file-name "eshell" prelude-savefile-dir))

(setq semanticdb-default-save-directory
      (expand-file-name "semanticdb" prelude-savefile-dir))

;; Compilation from Emacs
(defun ya/colorize-compilation-buffer ()
  "Colorize a compilation mode buffer."
  (interactive)
  ;; we don't want to mess with child modes such as grep-mode, ack, ag, etc
  (when (eq major-mode 'compilation-mode)
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max)))))

(require 'compile)
(setq
 ;; Just save before compiling
 compilation-ask-about-save nil
 ;; Just kill old compile processes before starting the new one
 compilation-always-kill t
 ;; Automatically scroll to first error
 compilation-scroll-output 'first-error)

;; Colorize output of Compilation Mode, see
;; http://stackoverflow.com/a/3072831/355252
(require 'ansi-color)
(add-hook 'compilation-filter-hook #'ya/colorize-compilation-buffer)

;; Enable our mode keybindings
(ya-global-mode t)

;; Do sensible undo
(global-undo-tree-mode)

;; Enable winner-mode to manage window configurations
(winner-mode +1)

;; Setup diff hihglights
(global-diff-hl-mode +1)
(add-hook 'dired-mode-hook 'diff-hl-dired-mode)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)


(defadvice server-visit-files (before parse-numbers-in-lines (files proc &optional nowait) activate)
  "Open file with emacsclient with cursors positioned on requested line.
Most of console-based utilities prints filename in format
'filename:linenumber'.  So you may wish to open filename in that format.
Just call:

  emacsclient filename:linenumber

and file 'filename' will be opened and cursor set on line 'linenumber'"
  (ad-set-arg 0
              (mapcar (lambda (fn)
                        (let ((name (car fn)))
                          (if (string-match "^\\(.*?\\):\\([0-9]+\\)\\(?::\\([0-9]+\\)\\)?$" name)
                              (cons
                               (match-string 1 name)
                               (cons (string-to-number (match-string 2 name))
                                     (string-to-number (or (match-string 3 name) ""))))
                            fn))) files)))

;; Use settings from .editorconfig file when present
(require 'editorconfig)
(editorconfig-mode 1)

;; KEYBINDINGS

(message "Init phase: keybindings")

;; Align your code in a pretty way.
(global-set-key (kbd "C-x \\") 'align-regexp)

;; Font size
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; Window switching. (C-x o goes to the next window)
(global-set-key (kbd "C-x O")
                (lambda ()
                  (interactive)
                  (other-window -1))) ;; Go back by one

;; Indentation help
(global-set-key (kbd "C-^") 'crux-top-join-line)

;; Start proced in a similar manner to dired
(unless (eq system-type 'darwin)
  (global-set-key (kbd "C-x p") 'proced))

;; Start eshell or switch to it if it's active.
(global-set-key (kbd "C-x m") 'eshell)

;; Start a new eshell even if one is active.
(global-set-key (kbd "C-x M") (lambda () (interactive) (eshell t)))

(define-key 'help-command (kbd "C-f") 'find-function)
(define-key 'help-command (kbd "C-k") 'find-function-on-key)
(define-key 'help-command (kbd "C-v") 'find-variable)
(define-key 'help-command (kbd "C-l") 'find-library)

(define-key 'help-command (kbd "C-i") 'info-display-manual)

;; Replace zap-to-char functionality with the more powerful zop-to-char
(global-set-key (kbd "M-z") 'zop-up-to-char)
(global-set-key (kbd "M-Z") 'zop-to-char)

;; kill lines backward
(global-set-key (kbd "C-<backspace>") (lambda ()
                                        (interactive)
                                        (kill-line 0)
                                        (indent-according-to-mode)))

(global-set-key [remap kill-whole-line] 'crux-kill-whole-line)

;; Activate occur easily inside isearch
(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)

;; Use hippie-expand instead of dabbrev
(global-set-key (kbd "M-/") 'hippie-expand)

;; Replace buffer-menu with ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Add magit commands
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

;; Add expand region
(global-set-key (kbd "C-=") 'er/expand-region)

;; Add avy goto subword
(global-set-key (kbd "s-.") 'avy-goto-word-or-subword-1)

;; Improve window navigation with ace-window
(global-set-key (kbd "s-w") 'ace-window)
(global-set-key [remap other-window] 'ace-window)

;; PLATFORMS

(message "Init phase: platforms")

;; TODO: define conditionally
;; TODO: define shortcut
(defun ya/swap-meta-and-super ()
  "Swap the mapping of Meta and Super.
Very useful for people using their Mac with a
Windows external keyboard from time to time."
  (interactive)
  (if (eq mac-command-modifier 'super)
      (progn
        (setq mac-command-modifier 'meta)
        (setq mac-option-modifier 'super)
        (message "Command is now bound to META and Option is bound to SUPER."))
    (setq mac-command-modifier 'super)
    (setq mac-option-modifier 'meta)
    (message "Command is now bound to SUPER and Option is bound to META.")))


(when (eq system-type 'darwin)
  (message "Loaging macOS specific tweaks")

  ;; Load PATH from shell
  (ya/require-packages '(exec-path-from-shell))
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize)

  ;; Set fn as function key
  (setq ns-function-modifier 'hyper)

  ;; There's no point in hiding the menu bar on macOS, so let's not do it
  (menu-bar-mode +1)

  ;; Enable emoji, and stop the UI from freezing when trying to display them.
  (when (fboundp 'set-fontset-font)
    (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend)))

;; EMACS CUSTOM FILE

(message "Init phase: custom.el")

;; Store config changes made through the customize UI here
(setq custom-file (expand-file-name "custom.el" ya/dir-root))

;; DONE

(message "Emacs ready!")
