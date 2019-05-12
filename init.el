;;; init.el --- My humble emacs config

;;; Commentary:

;; This is a `use-package' and `straight.el' based config focused on
;; "minimal" functionality and good startup time.

;;; Code:

;; Always load newest byte code
(setq load-prefer-newer t)

(defun megabytes (n)
  "Return the number of bytes in N megabytes."
  (* n 1024 1024))

;; Increase GC threshold to 50MB for better throughput during init
(setq gc-cons-threshold (megabytes 50))

;; Increase large file limit to 50MB
(setq large-file-warning-threshold (megabytes 50))

;; Define directories

(defvar ya/dir-root (file-name-directory load-file-name)
  "The root dir of this config.")

(defvar ya/dir-lisp (expand-file-name "lisp" ya/dir-root)
  "This directory stores custom Lisp code.")
(add-to-list 'load-path ya/dir-lisp)

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

;; Don't highlight the current line
(global-hl-line-mode -1)

;; Show line numbers
(global-display-line-numbers-mode t)

;; Show line and column numbers, and file size indication in the
;; modeline
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; Disable sounds, startup screen, prettify scrolling
(setq ring-bell-function 'ignore
      inhibit-startup-screen t
      scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position t)


;; Save whatever’s in the current (system) clipboard before
;; replacing it with the Emacs’ text.
(setq save-interprogram-paste-before-kill t)

;; Enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; Have more useful frame title. Show either a file or a
;; buffer name, if the buffer isn't visiting a file.
(setq frame-title-format
      '("" invocation-name " - "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name)) "%b"))))

;;;; Customize editor behavior

;; Smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

;; Don't use tabs to indent, but set them to appear at 4 spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default tab-stop-list (number-sequence 4 120 4))

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

;;;; Misc customizations

;; Store all backup and autosave files in the tmp dir
(setq backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Revert buffers automatically when underlying files are changed externally
;; This still prompts for confirmation if buffer has unsaved changes
(global-auto-revert-mode t)

;;;; Bootstrap straight.el

;; This produces a huge (2.5x) init time perf boost by not using
;; find(1) on startup to detect whether a package needs rebuilding at
;; init time.
(setq straight-check-for-modifications '(check-on-save find-when-checking))

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

;; PERF ~90ms
(use-package doom-themes
  :straight t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-one t))

;; Make unique and more meaningful names for buffers with the same name
(use-package uniquify
  :straight f
  :config
  (setq uniquify-buffer-name-style 'forward
        uniquify-separator "/"
        ;; Rename after killing uniquified
        uniquify-after-kill-buffer-p t
        ;; Ignore special buffers
        uniquify-ignore-buffers-re "^\\*"))

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

(use-package undo-tree
  :straight t
  :bind (("C-/" . undo-tree-undo)
         ("C-?" . undo-tree-redo)
         ("C-c u" . undo-tree-visualize))
  :config
  (global-undo-tree-mode 1))

;; PERF ~80ms
;; Note: ivy has :demand, because projectile doesn't trigger
;; an autoload of ivy with `projectile-add-known-project'
(use-package ivy
  :straight t
  :demand
  :bind (("C-x b" . ivy-switch-buffer)
         ("C-x 4 b" . ivy-switch-buffer-other-window))
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
         ("M-y" . counsel-yank-pop)
         ("C-c s g" . counsel-git-grep)
         ("C-c s r" . counsel-rg)))

(use-package projectile
  :straight t
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

(use-package neotree
  :straight t
  :bind (("s-t" . neotree-toggle)
         ("C-c t" . neotree-toggle))
  :config
  (setq neo-theme 'arrow
        neo-smart-open t))

(use-package magit
  :straight t
  :bind (("s-g g" . magit-status)
         ("s-g l" . magit-log)
         ("s-g f" . magit-log-buffer-file)
         ("s-g b" . magit-blame)
         ("C-c g m" . magit-status)
         ("C-c g l" . magit-log)
         ("C-c g f" . magit-log-buffer-file)
         ("C-c g b" . magit-blame)))

(use-package git-timemachine
  :straight t
  :commands git-timemachine)

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
  :bind (("C-c m l" . mc/edit-lines)
         ("C-c m c" . mc/mark-all-dwim)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/unmark-next-like-this)))

(use-package editorconfig
  :straight t
  :config
  (editorconfig-mode 1))

(use-package expand-region
  :straight t
  :bind ("C-:" . er/expand-region))

(use-package avy
  :straight t
  :bind (("C-;" . avy-goto-char)
         ("C-'" . avy-goto-word-1)))

;; Improve window navigation with ace-window
(use-package ace-window
  :straight t
  :bind ("C-x o" . ace-window))

(use-package whitespace
  :straight f
  :config
  (setq whitespace-line-column 100
        whitespace-style '(face tabs empty trailing lines-tail))
  (add-hook 'before-save-hook 'whitespace-cleanup nil t)
  (global-whitespace-mode +1))

;; Have saner regex syntax
(use-package re-builder
  :straight f
  :config
  (setq reb-re-syntax 'string))

;; PERF ~20ms
(use-package diff-hl
  :straight t
  :config
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (global-diff-hl-mode +1))

(use-package flycheck
  :straight t
  :hook (prog-mode . flycheck-mode))

(use-package smartparens
  :straight t
  :hook (prog-mode . smartparens-mode)
  :config
  (require 'smartparens-config)
  (setq sp-base-key-bindings 'paredit
        sp-autoskip-closing-pair 'always
        sp-hybrid-kill-entire-symbol nil)
  (sp-use-paredit-bindings)
  ;; Show matching parens everywhere this is hooked
  (show-smartparens-global-mode +1)
  (sp-pair "{" nil :post-handlers
           '(((lambda (&rest _ignored)
                (crux-smart-open-line-above)) "RET"))))

;; LSP client

(use-package lsp-mode
  :straight t
  :commands lsp
  :hook (rust-mode . lsp)
  :config
  (setq lsp-enable-snippet nil)
  (require 'ra-emacs-lsp))

(use-package lsp-ui
  :commands lsp-ui-mode)

(use-package company-lsp
  :commands company-lsp)

;;;; Rust

(use-package rust-mode
  :straight t
  :mode ("\\.rs\\'" . rust-mode)
  :config
  (setq rust-format-on-save nil)
  (defun setup-rust-mode ()
    (subword-mode +1))
  (add-hook 'rust-mode-hook 'setup-rust-mode))

(use-package flycheck-rust
  :straight t
  :after flycheck ;; TODO: audit if :after works here or this is actually fragile
  :hook (rust-mode . flycheck-rust-setup))

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
(setq gc-cons-threshold (megabytes 1))

;; TODO:
;; Rust RLS with lsp-mode? (Using Rust Analyzer currently)
;; Typescript with lsp-mode?
;; Spelling correction: flyspell
;; Haskell: haskell-mode
