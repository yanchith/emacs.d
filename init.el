;; Always load newest byte code
(setq load-prefer-newer t)

;; Increase GC threshold to 50MB for better throughput during init
(setq gc-cons-threshold 50000000)

;; Increase large file limit to 50MB
(setq large-file-warning-threshold 50000000)

;; Define directories

(defvar ya/dir-root (file-name-directory load-file-name)
  "The root dir of this config.")

(defvar ya/dir-savefile (expand-file-name "savefile" ya/dir-root)
  "This directory stores all automatically generated save/history files.")
(unless (file-exists-p ya/dir-savefile)
  (make-directory ya/dir-savefile))

;; Store config changes made through the customize UI here
(setq custom-file (expand-file-name "custom.el" ya/dir-root))

;;;; Customize UI

;; Hide all the bars
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; PERF The emacs default for initial-major-mode is
;; lisp-interaction-mode, which is a programming mode. Everything
;; :hook-ed with prog-mode (such as the very expensive flycheck) would
;; be loaded right from the start (bad for perf), if we kept it that
;; way.
(setq initial-major-mode 'fundamental-mode
      initial-scratch-message ";; Welcome to the scratch buffer\n\n")

;; Disable cursor blinking
(blink-cursor-mode -1)

;; Always highlight the current line
(global-hl-line-mode +1)

;; Disable sounds, startup screen, prettify scrolling
(setq ring-bell-function 'ignore
      inhibit-startup-screen t
      scroll-margin 0
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

;;;; Customize editor behavior

;; Smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

;; Don't use tabs to indent, but set them to appear at 4 spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
;; Change C indentation to respect our tab width
(defvaralias 'c-basic-offset 'tab-width)

;; Newline at end of file
(setq require-final-newline t)

;; Delete the selection with a keypress
(delete-selection-mode t)

;;;; Define keybindings for common operations

;; Font size
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; Replace buffer-menu with ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Define additional help functions
(define-key 'help-command (kbd "C-f") 'find-function)
(define-key 'help-command (kbd "C-k") 'find-function-on-key)
(define-key 'help-command (kbd "C-v") 'find-variable)
(define-key 'help-command (kbd "C-l") 'find-library)
(define-key 'help-command (kbd "C-i") 'info-display-manual)

;;;; Misc customizations

;; Store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Revert buffers automatically when underlying files are changed externally
;; This still prompts for confirmation if buffer has unsaved changes
(global-auto-revert-mode t)

;;;; Bootstrap straight.el

;; This produces a huge (2.5x) init time perf boost by not using find(1)
;; to detect whether a package needs rebuilding at init time.
(setq straight-check-for-modifications nil)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;;; Bootstrap use-package

(straight-use-package 'use-package)
(eval-when-compile (require 'use-package))

;;;; Configure packages

;; PERF ~70ms
(use-package doom-themes
  :straight t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-one t))

;; Make unique and more meaningful names for buffers with the same name
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward
        uniquify-separator "/"
        ;; Rename after killing uniquified
        uniquify-after-kill-buffer-p t
        ;; Ignore special buffers
        uniquify-ignore-buffers-re "^\\*"))

;; Show available keybindings after you start typing
(use-package which-key
  :straight t
  :config
  (which-key-mode +1))

(use-package delight
  :straight t)

(use-package crux
  :straight t
  :bind (("C-a" . crux-move-beginning-of-line)

         ("s-j" . crux-top-join-line)
         ("s-k" . crux-kill-whole-line)

         ("C-c r" . crux-rename-buffer-and-file)
         ("C-c k" . crux-kill-other-buffers)

         ("C-c d" . crux-duplicate-current-line-or-region)))

(use-package move-text
  :straight t
  :bind (("M-p" . move-text-up)
         ("M-n" . move-text-down)))

(use-package ivy
  :straight t
  :delight ivy-mode
  :bind ("C-x b" . ivy-switch-buffer)
  :config
  (setq ivy-use-virtual-buffers t
        enable-recursive-minibuffers t)
  (ivy-mode 1))

;; swiper provides enhanced buffer search, replace i-search with swiper
(use-package swiper
  :straight t
  :bind ("C-s" . swiper))

;; counsel supercharges a lot of commands with some ivy magic
(use-package counsel
  :straight t
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-c g" . counsel-git)
         ("C-c s g" . counsel-git-grep)
         ("C-c s r" . counsel-rg)))

(use-package projectile
  :straight t
  :delight '(:eval (concat " [" (projectile-project-name) "]"))
  :bind (("s-p p" . projectile-switch-project)
         ("s-p f" . projectile-find-file)
         ("s-p a" . projectile-add-known-project)
         ("s-p r" . projectile-remove-known-project)
         ("C-c p p" . projectile-switch-project)
         ("C-c p f" . projectile-find-file)
         ("C-c p a" . projectile-add-known-project)
         ("C-c p r" . projectile-remove-known-project))
  :config
  (setq projectile-cache-file (expand-file-name  "projectile.cache" ya/dir-savefile)
        projectile-completion-system 'ivy)
  (projectile-mode t))

(use-package magit
  :straight t
  :bind (("s-m m" . magit-status)
         ("s-m l" . magit-log)
         ("s-m f" . magit-log-buffer-file)
         ("s-m b" . magit-blame)
         ("C-c m m" . magit-status)
         ("C-c m l" . magit-log)
         ("C-c m f" . magit-log-buffer-file)
         ("C-c m b" . magit-blame)))

;; TODO: :bind this so it can be deferred
(use-package git-timemachine
    :straight t)

;; Set up completetions in text
(use-package company
  :straight t
  :hook (prog-mode . company-mode)
  :config
  (setq company-idle-delay 0.2
        company-show-numbers t
        company-tooltip-limit 10
        company-minimum-prefix-length 2
        company-tooltip-align-annotations t))

(use-package multiple-cursors
  :straight t
  :bind (("C-c l l" . mc/edit-lines)
         ("C-c l a" . mc/mark-all-like-this-dwim)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/unmark-next-like-this)))

(use-package editorconfig
  :straight t
  :delight (editorconfig-mode " EC")
  :config
  (editorconfig-mode 1))

(use-package expand-region
  :straight t
  :bind ("C-:" . er/expand-region))

;; Improve window navigation with ace-window
(use-package ace-window
  :straight t
  :bind ([remap other-window] . ace-window))

;; TODO dired-subtree
;; Enable some really cool extensions like C-x C-j(dired-jump)
(use-package dired-x
  :config
  ;; Auto refresh dired when file changes
  (add-hook 'dired-mode-hook 'auto-revert-mode)
  ;; Always delete and copy recursively
  (setq dired-recursive-deletes 'always
        dired-recursive-copies 'always))

;; ediff - don't start another frame
(use-package ediff
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

;; Whitespace-mode config
(use-package whitespace
  :config
  (setq whitespace-line-column 100
        whitespace-style '(face tabs empty trailing lines-tail))
  (add-hook 'before-save-hook 'whitespace-cleanup nil t)
  (global-whitespace-mode +1))

;; Have saner regex syntax
(use-package re-builder
  :config
  (setq reb-re-syntax 'string))

(use-package eshell
  :bind (("C-x m" . eshell)
         ("C-x M" . (lambda () (interactive) (eshell t))))
  :config
  (setq eshell-directory-name (expand-file-name "eshell" ya/dir-savefile)))

;; TODO: what is semanticdb? audit
(setq semanticdb-default-save-directory
      (expand-file-name "semanticdb" ya/dir-savefile))

;; Enable winner-mode to manage window configurations
(winner-mode +1)

(use-package diff-hl
  :straight t
  :config
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (global-diff-hl-mode +1))

(use-package flycheck
  :straight t
  :hook (prog-mode . flycheck-mode))

;; TODO: :hook into prog-mode to defer
;; PERF ~40ms
(use-package smartparens-config
  :straight smartparens
  :config
  (setq sp-base-key-bindings 'paredit
        sp-autoskip-closing-pair 'always
        sp-hybrid-kill-entire-symbol nil) ;; TODO: audit
  (sp-use-paredit-bindings)
  (sp-pair "{" nil :post-handlers
           '(((lambda (&rest _ignored)
                (crux-smart-open-line-above)) "RET")))
  (smartparens-global-mode +1)
  (show-smartparens-global-mode +1))

;; show the name of the current function definition in the modeline
(use-package which-func
  :config
  (which-function-mode 1))

;;;; Rust

;; The following packages may need to be installed manually on the system
;; * rustc (Rust Compiler)
;; * cargo (Rust Package Manager)
;; * racer (Rust Completion Tool)
;; * rustfmt (Rust Tool for formatting code)

;; TODO: :mode this to defer if possible
(use-package rust-mode
  :straight t
  :mode ("\\.rs\\'" . rust-mode)
  :config
  (setq rust-format-on-save t)
  (defun setup-rust-mode ()
    (subword-mode +1))
  (add-hook 'rust-mode-hook 'setup-rust-mode))

(use-package flycheck-rust
  :straight t
  :after flycheck
  :hook (rust-mode . flycheck-rust-setup))

(use-package racer
  :straight t
  :hook (rust-mode . racer-mode)
  :config
  ;; TODO: should be in config.el
  (setq racer-rust-src-path "~/.rustup/toolchains/stable-x86_64-apple-darwin/lib/rustlib/src/rust/src")
  (add-hook 'racer-mode-hook 'eldoc-mode))

(use-package cargo
  :straight t
  :hook (rust-mode . cargo-minor-mode))

;;;; TypeScript

(use-package typescript-mode
  :straight tide
  :mode ("\\.tsx?\\'" . typescript-mode)
  :config
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (subword-mode +1))
  (add-hook 'typescript-mode-hook 'setup-tide-mode))

;;;; GLSL

(use-package glsl-mode
  :straight t
  :mode (("\\.glsl\\'" . glsl-mode)
         ("\\.vert\\'" . glsl-mode)
         ("\\.frag\\'" . glsl-mode)
         ("\\.geom\\'" . glsl-mode))
  )

;;;; Racket

(use-package racket-mode
  :straight t
  :mode ("\\.rkt\\'" . racket-mode)
  :config
  (defun setup-racket-mode ()
    (smartparens-strict-mode +1))
  (add-hook 'racket-mode-hook 'setup-racket-mode))

;;;; Markdown

(use-package markdown-mode
  :straight t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (setq markdown-command "multimarkdown"))

;;;; macOS

(use-package exec-path-from-shell
  :if (eq system-type 'darwin)
  :straight t
  :config
  (exec-path-from-shell-initialize))

(when (eq system-type 'darwin)
  ;; Set option/alt -> meta, cmd -> super and fn -> hyper
  (setq mac-option-key-is-meta t
        mac-command-key-is-meta nil
        mac-command-modifier 'super
        mac-option-modifier 'meta
        ns-function-modifier 'hyper))

;; Restore gc threshold for better interactivity and shorter pauses
(setq gc-cons-threshold 800000)

;; TODO:
;; Rust RLS with lsp-mode?
;; Typescript with lsp-mode?
;; Spelling correction: flyspell
;; Editing: recentf, savehist
;; Haskell: haskell-mode
