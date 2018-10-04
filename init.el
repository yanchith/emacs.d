(message "Emacs is powering up...")

;; Always load newest byte code
(setq load-prefer-newer t)

;; Increase GC threshold to 50MB for better throughput during init
(setq gc-cons-threshold 50000000)

;; Increase large file limit to 100MB
(setq large-file-warning-threshold 100000000)

;; Define directories

(defvar ya/dir-root (file-name-directory load-file-name)
  "The root dir of this config.")

(defvar ya/dir-savefile (expand-file-name "savefile" ya/dir-root)
  "This directory stores all automatically generated save/history files.")

(defvar ya/dir-packages-elpa (expand-file-name "packages-elpa" ya/dir-root)
  "This directory stores all downloaded packages.")

(require 'package)

;; Add MELPA to gain access to more packages
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; Set package-user-dir path, initialize package and refresh index
(setq package-user-dir ya/dir-packages-elpa)
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

;; Give melpa-stable higher priority
(setq package-archive-priorities '(("melpa-stable" . 10) ("melpa" . 5)))

;; TODO: package lockfile

(defun ya/install (pkg)
  "Install PKG unless already installed."
  (when (not (package-installed-p pkg))
    (package-install pkg)))

;; TODO: configure these packages
;; --JSON
;; json-mode

;; --Web (HTML + CSS + JS)
;; web-mode

;; --JavaScript
;; js2-mode

;; --TypeScript
;; tide

;; --Rust
;; rust-mode
;; racer
;; flycheck-rust
;; cargo

;; --Haskell
;; haskell-mode

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

(ya/install 'doom-themes)
(load-theme 'doom-one t)
(setq doom-themes-enable-bold t
      doom-themes-enable-italic t)

;; Show available keybindings after you start typing
(ya/install 'which-key)
(require 'which-key)
(which-key-mode +1)

(ya/install 'crux)
(require 'crux)

(global-set-key (kbd "C-a") 'crux-move-beginning-of-line)

(global-set-key (kbd "M-o") 'crux-smart-open-line)
(global-set-key (kbd "s-o") 'crux-smart-open-line-above)
(global-set-key (kbd "C-c d") 'crux-duplicate-current-line-or-region)
(global-set-key (kbd "C-c M-d") 'crux-duplicate-and-comment-current-line-or-region)
(global-set-key (kbd "s-j") 'crux-top-join-line)
(global-set-key (kbd "s-k") 'crux-kill-whole-line)

(global-set-key (kbd "C-c f") 'crux-recentf-ido-find-file)
(global-set-key (kbd "s-r") 'crux-recentf-ido-find-file)
(global-set-key (kbd "C-c r") 'crux-rename-buffer-and-file)
(global-set-key (kbd "C-c k") 'crux-kill-other-buffers)
(global-set-key (kbd "C-c I") 'crux-find-user-init-file)
(global-set-key (kbd "C-c S") 'crux-find-shell-init-file)

(ya/install 'move-text)
(global-set-key (kbd "M-p")  'move-text-up)
(global-set-key (kbd "M-n")  'move-text-down)

(ya/install 'projectile)
(require 'projectile)
(setq projectile-cache-file (expand-file-name  "projectile.cache" ya/dir-savefile))
(projectile-mode t)

(global-set-key (kbd "s-p") 'projectile-command-map)
(global-set-key (kbd "C-c p") 'projectile-command-map)

(ya/install 'magit)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)
(global-set-key (kbd "s-m m") 'magit-status)
(global-set-key (kbd "s-m l") 'magit-log)
(global-set-key (kbd "s-m f") 'magit-log-buffer-file)
(global-set-key (kbd "s-m b") 'magit-blame)

(ya/install 'git-timemachine)

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

(ya/install 'undo-tree)
;; Autosave the undo-tree history
(setq undo-tree-history-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq undo-tree-auto-save-history t)
(global-undo-tree-mode)

;; TODO: these probably require more configuring
(ya/install 'ivy)
(ya/install 'swiper)
(ya/install 'counsel)
(ya/install 'company)

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

;; saveplace remembers your location in a file when saving files
(setq save-place-file (expand-file-name "saveplace" ya/dir-savefile))
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
      savehist-file (expand-file-name "savehist" ya/dir-savefile))
(savehist-mode +1)

;; save recent files
(require 'recentf)
(setq recentf-save-file (expand-file-name "recentf" ya/dir-savefile)
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
             (mapcar 'file-truename (list ya/dir-savefile package-user-dir)))))

(add-to-list 'recentf-exclude 'prelude-recentf-exclude-p)

(recentf-mode +1)


(defadvice set-buffer-major-mode (after set-major-mode activate compile)
  "Set buffer major mode according to `auto-mode-alist'."
  (let* ((name (buffer-name buffer))
         (mode (assoc-default name auto-mode-alist 'string-match)))
    (when (and mode (consp mode))
      (setq mode (car mode)))
    (with-current-buffer buffer (if mode (funcall mode)))))

;; highlight the current line
(global-hl-line-mode +1)

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

;; avy allows us to effectively navigate to visible things
(ya/install 'avy)
(require 'avy)
(setq avy-background t)
(setq avy-style 'at-full)
(global-set-key (kbd "s-.") 'avy-goto-word-or-subword-1)

(ya/install 'multiple-cursors)
(require 'multiple-cursors)
(global-set-key (kbd "C-c m c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
;; TODO mc/mark-all-like-this

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
(setq eshell-directory-name (expand-file-name "eshell" ya/dir-savefile))

;; TODO: what is semanticdb?
(setq semanticdb-default-save-directory
      (expand-file-name "semanticdb" ya/dir-savefile))

;; TODO: what is this?
(require 'compile)
(setq
 ;; Just save before compiling
 compilation-ask-about-save nil
 ;; Just kill old compile processes before starting the new one
 compilation-always-kill t
 ;; Automatically scroll to first error
 compilation-scroll-output 'first-error)

;; Enable winner-mode to manage window configurations
(winner-mode +1)

(ya/install 'diff-hl)
(global-diff-hl-mode +1)
(add-hook 'dired-mode-hook 'diff-hl-dired-mode)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

(ya/install 'editorconfig)
(require 'editorconfig)
(editorconfig-mode 1)

;; Font size
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; Start eshell or switch to it if it's active.
(global-set-key (kbd "C-x m") 'eshell)

;; Start a new eshell even if one is active.
(global-set-key (kbd "C-x M") (lambda () (interactive) (eshell t)))

(define-key 'help-command (kbd "C-f") 'find-function)
(define-key 'help-command (kbd "C-k") 'find-function-on-key)
(define-key 'help-command (kbd "C-v") 'find-variable)
(define-key 'help-command (kbd "C-l") 'find-library)

(define-key 'help-command (kbd "C-i") 'info-display-manual)

;; kill lines backward
(global-set-key (kbd "C-<backspace>")
                (lambda ()
                  (interactive)
                  (kill-line 0)
                  (indent-according-to-mode)))

;; Use hippie-expand instead of dabbrev
(global-set-key (kbd "M-/") 'hippie-expand)

;; Replace buffer-menu with ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

(ya/install 'expand-region)
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; Improve window navigation with ace-window
(ya/install 'ace-window)
(global-set-key (kbd "s-w") 'ace-window)
(global-set-key [remap other-window] 'ace-window)

(when (eq system-type 'darwin)
  ;; Load PATH from shell
  (ya/install 'exec-path-from-shell)
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize)

  ;; Set fn as function key
  (setq ns-function-modifier 'hyper))

;; Store config changes made through the customize UI here
(setq custom-file (expand-file-name "custom.el" ya/dir-root))

;; Restore gc threshold for better interactivity and shorter pauses
(setq gc-cons-threshold 800000)

(message "Done!")
