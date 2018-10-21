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
(unless (file-exists-p ya/dir-savefile)
  (make-directory ya/dir-savefile))

(defvar ya/dir-packages-elpa (expand-file-name "packages-elpa" ya/dir-root)
  "This directory stores all downloaded packages.")

;; Store config changes made through the customize UI here
(setq custom-file (expand-file-name "custom.el" ya/dir-root))

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

(defun ya/install (pkg)
  "Install PKG unless already installed."
  (when (not (package-installed-p pkg))
    (package-install pkg)))

;; Reclaim some screen real-estate
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(menu-bar-mode -1)
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; Disable cursor blinking
(blink-cursor-mode -1)

;; Always highlight the current line
(global-hl-line-mode +1)

;; Disable sounds
(setq ring-bell-function 'ignore)

;; Disable startup screen
(setq inhibit-startup-screen t)

;; Add nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position t)

;; Show line and column numbers, and file size indication
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; Save whatever’s in the current (system) clipboard before
;; replacing it with the Emacs’ text.
(setq save-interprogram-paste-before-kill t)

;; Enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; Add more useful frame title, Show either a file or a
;; buffer name (if the buffer isn't visiting a file)
;; TODO: I broke this... fix
(setq frame-title-format
      '("" invocation-name ""
        (:eval (if (buffer-fiLe-name)
                   (abbreviate-file-name (buffer-file-name)) "%b"))))

(ya/install 'doom-themes)
(load-theme 'doom-one t) ;; TODO: audit (there are theme-specific settings)
(setq doom-themes-enable-bold t
      doom-themes-enable-italic t)


;; meaningful names for buffers with the same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; ignore special buffers

;; Show available keybindings after you start typing
(ya/install 'which-key)
(require 'which-key)
(which-key-mode +1)

(ya/install 'crux)
(require 'crux)

(global-set-key (kbd "C-a") 'crux-move-beginning-of-line)

(global-set-key (kbd "M-o") 'crux-smart-open-line)
(global-set-key (kbd "s-o") 'crux-smart-open-line-above)
(global-set-key (kbd "s-j") 'crux-top-join-line)
(global-set-key (kbd "s-k") 'crux-kill-whole-line)

(global-set-key (kbd "C-c r") 'crux-rename-buffer-and-file)
(global-set-key (kbd "C-c k") 'crux-kill-other-buffers)

(global-set-key (kbd "C-c d") 'crux-duplicate-current-line-or-region)

(ya/install 'move-text)
(global-set-key (kbd "M-p")  'move-text-up)
(global-set-key (kbd "M-n")  'move-text-down)

(ya/install 'projectile)
(require 'projectile)
(setq projectile-cache-file (expand-file-name  "projectile.cache" ya/dir-savefile))
(projectile-mode t)

(global-set-key (kbd "s-p") 'projectile-command-map)

(ya/install 'magit)
(global-set-key (kbd "s-m m") 'magit-status)
(global-set-key (kbd "s-m l") 'magit-log)
(global-set-key (kbd "s-m f") 'magit-log-buffer-file)
(global-set-key (kbd "s-m b") 'magit-blame)

(ya/install 'git-timemachine)

;; TODO: audit
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

;; Set up ivy, swiper and counsel
(ya/install 'ivy)
(ya/install 'swiper)
(ya/install 'counsel)

(require 'ivy)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)

;; Integrate ivy with projectile
(setq projectile-completion-system 'ivy)

;; swiper provides enhanced buffer search, replace i-search with swiper
(global-set-key "\C-s" 'swiper)

;; counsel supercharges a lot of commands with some ivy magic
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c s g") 'counsel-git-grep)
(global-set-key (kbd "C-c s r") 'counsel-rg)

(ya/install 'company)
(require 'company)

;; Set up completetions in text, TODO: audit
(setq company-idle-delay 0.3)
(setq company-show-numbers t)
(setq company-tooltip-limit 10)
(setq company-minimum-prefix-length 2)
(setq company-tooltip-align-annotations t)
(global-company-mode 1)

;; avy allows us to effectively navigate to visible things
(ya/install 'avy)
(require 'avy)
(setq avy-background t)
(setq avy-style 'at-full)
(global-set-key (kbd "s-.") 'avy-goto-word-or-subword-1)

(ya/install 'multiple-cursors)
(require 'multiple-cursors)
(global-set-key (kbd "C-c m c") 'mc/edit-lines)
(global-set-key (kbd "C-c C->") 'mc/mark-all-like-this)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/unmark-next-like-this)

(ya/install 'editorconfig)
(require 'editorconfig)
(editorconfig-mode 1)

(ya/install 'expand-region)
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; Improve window navigation with ace-window
(ya/install 'ace-window)
(global-set-key [remap other-window] 'ace-window)

;; Smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

;; saveplace remembers your location in a file when saving files activate it
;; for all buffers
(setq save-place-file (expand-file-name "saveplace" ya/dir-savefile))
(save-place-mode 1)

;; Always delete and copy recursively
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)

;; TODO dired-subtree

;; Enable some really cool extensions like C-x C-j(dired-jump)
(require 'dired-x)

;; Revert buffers automatically when underlying files are changed externally
;; This still prompts for confirmation if buffer has unsaved changes
(global-auto-revert-mode t)
;; auto refresh dired when file changes
(add-hook 'dired-mode-hook 'auto-revert-mode)

;; ediff - don't start another frame
(require 'ediff)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Whitespace-mode config
(require 'whitespace)
(setq whitespace-line-column 80)
(setq whitespace-style '(face tabs empty trailing lines-tail))
(add-hook 'before-save-hook 'whitespace-cleanup nil t)
(global-whitespace-mode +1)

;; Have saner regex syntax
(require 're-builder)
(setq reb-re-syntax 'string)

(require 'eshell)
(setq eshell-directory-name (expand-file-name "eshell" ya/dir-savefile))
;; Start eshell or switch to it if it's active.
(global-set-key (kbd "C-x m") 'eshell)
;; Start a new eshell even if one is active.
(global-set-key (kbd "C-x M") (lambda () (interactive) (eshell t)))

;; TODO: what is semanticdb? audit
(setq semanticdb-default-save-directory
      (expand-file-name "semanticdb" ya/dir-savefile))

;; Enable winner-mode to manage window configurations
(winner-mode +1)

(ya/install 'diff-hl)
(global-diff-hl-mode +1)
(add-hook 'dired-mode-hook 'diff-hl-dired-mode)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

;; Font size
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; TODO: audit - isn't this the default?
(define-key 'help-command (kbd "C-f") 'find-function)
(define-key 'help-command (kbd "C-k") 'find-function-on-key)
(define-key 'help-command (kbd "C-v") 'find-variable)
(define-key 'help-command (kbd "C-l") 'find-library)

(define-key 'help-command (kbd "C-i") 'info-display-manual)

;; kill lines backward, TODO: audit - isn't this the default?
(global-set-key (kbd "C-<backspace>")
                (lambda ()
                  (interactive)
                  (kill-line 0)
                  (indent-according-to-mode)))

;; Replace buffer-menu with ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; General programming support

(ya/install 'flycheck)
(ya/install 'smartparens)

(require 'smartparens-config)
(setq sp-base-key-bindings 'paredit)
(setq sp-autoskip-closing-pair 'always)
(setq sp-hybrid-kill-entire-symbol nil) ;; TODO: audit
(sp-use-paredit-bindings)
(sp-pair "{" nil :post-handlers
         '(((lambda (&rest _ignored)
              (crux-smart-open-line-above)) "RET")))

(smartparens-global-mode +1)
(show-smartparens-global-mode +1)

;; TODO comment filling and autofilling

;; show the name of the current function definition in the modeline
(require 'which-func)
(which-function-mode 1)

;; enable on-the-fly syntax checking
(if (fboundp 'global-flycheck-mode)
    (global-flycheck-mode +1)
  (add-hook 'prog-mode-hook 'flycheck-mode))

;; Setup rust support
;; You may need installing the following packages on your system:
;; * rustc (Rust Compiler)
;; * cargo (Rust Package Manager)
;; * racer (Rust Completion Tool)
;; * rustfmt (Rust Tool for formatting code)

(ya/install 'rust-mode)
(ya/install 'racer)
(ya/install 'flycheck-rust)
(ya/install 'cargo)

(setq rust-format-on-save t)
;; TODO: should be in config.el
(setq racer-rust-src-path "~/.rustup/toolchains/beta-x86_64-apple-darwin/lib/rustlib/src/rust/src")

(eval-after-load 'rust-mode
  '(progn
     (add-hook 'rust-mode-hook 'racer-mode)
     (add-hook 'racer-mode-hook 'eldoc-mode)
     (add-hook 'rust-mode-hook 'cargo-minor-mode)
     (add-hook 'rust-mode-hook 'flycheck-rust-setup)
     (add-hook 'flycheck-mode-hook 'flycheck-rust-setup)

     (defun ya/rust-mode-hook ()
       (local-set-key (kbd "C-c C-d") 'racer-describe)
       ;; CamelCase aware editing operations
       (subword-mode +1))

     (add-hook 'rust-mode-hook 'ya/rust-mode-hook)))


(when (eq system-type 'darwin)
  ;; Load PATH from shell
  (ya/install 'exec-path-from-shell)
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize)

  ;; Set option/alt -> meta and cmd -> super
  (setq mac-option-key-is-meta t)
  (setq mac-command-key-is-meta nil)
  (setq mac-command-modifier 'super)
  (setq mac-option-modifier 'meta)

  ;; Set fn as function key
  (setq ns-function-modifier 'hyper))

;; Restore gc threshold for better interactivity and shorter pauses
(setq gc-cons-threshold 800000)

(message "Done!")

;; TODO:
;; better package management: use-package
;; Spelling correction: flyspell
;; Editing: recentf, savehist
;; JSON: json-mode
;; Web (HTML/CSS/JS): web-mode
;; JavaScript: js2-mode
;; TypeScript: tide
;; Haskell: haskell-mode
