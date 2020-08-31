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

(defvar yanchith-dir-root (file-name-directory load-file-name)
  "The root dir of this config.")

(defvar yanchith-dir-lisp (expand-file-name "lisp" yanchith-dir-root)
  "This directory stores custom Lisp code.")
(add-to-list 'load-path yanchith-dir-lisp)

(defvar yanchith-dir-savefile (expand-file-name "savefile" yanchith-dir-root)
  "This directory stores all automatically generated save/history files.")
(unless (file-exists-p yanchith-dir-savefile)
  (make-directory yanchith-dir-savefile))

;; Store config changes made through the customize UI here
(setq custom-file (expand-file-name "custom.el" yanchith-dir-root))

;;;; Customize UI

;; Set the default font

(cond ((eq system-type 'darwin) (set-frame-font "Liberation Mono-14"))
      ((eq system-type 'windows-nt) (set-frame-font "Liberation Mono-11"))
      ((eq system-type 'gnu/linux) (set-frame-font "Liberation Mono-11")))

;; Do handmade-ish color theme

(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; TODO: Configure faces for ivy/counsel, company and magit to be more in line
;; with this style

(set-foreground-color "burlywood3")
(set-background-color "#161616")
(set-cursor-color "#40FF40")

(set-face-attribute 'font-lock-comment-face nil :foreground "gray50" :weight 'normal)
(set-face-attribute 'font-lock-doc-face nil :foreground "gray70" :weight 'normal)

(set-face-attribute 'font-lock-string-face nil :foreground "olive drab" :weight 'normal)

;; Highlight things: warnings > builtins > keywords > types > other
(set-face-attribute 'font-lock-warning-face nil :foreground "DarkGoldenrod1" :weight 'bold)
(set-face-attribute 'font-lock-builtin-face nil :foreground "DarkGoldenrod3" :weight 'bold)
(set-face-attribute 'font-lock-keyword-face nil :foreground "DarkGoldenrod3" :weight 'normal)
(set-face-attribute 'font-lock-type-face nil :foreground "burlywood3" :weight 'normal)
(set-face-attribute 'font-lock-constant-face nil :foreground "burlywood3" :weight 'normal)
(set-face-attribute 'font-lock-function-name-face nil :foreground "burlywood3" :weight 'normal)
(set-face-attribute 'font-lock-variable-name-face nil :foreground "burlywood3" :weight 'normal)


;; PERF: The emacs default for initial-major-mode is
;; lisp-interaction-mode, which is a programming mode. Everything
;; :hook-ed with prog-mode (such as the very expensive flycheck) would
;; be loaded right from the start (bad), if we kept it that way.
(setq initial-major-mode 'fundamental-mode
      initial-scratch-message "The Spice must flow...\n\n")

;; Disable cursor blinking
(blink-cursor-mode -1)

;; Don't blink matching parenthesis by default as it has a painfully slow
;; interaction with multiple cursors when inserting closing
;; parenthesis. Instead, we highlight the matching paren with show-paren-mode.
;; PERF: show-paren-mode is built into emacs and implemented in C. While
;; `show-smartparens-mode' is more clever, it is also more laggy.
(setq blink-matching-paren nil)
(setq-default show-paren-delay 0)
(show-paren-mode)

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

;; Give comments more space

(setq-default fill-column 80)

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

;;;; Disable some enabled-by-default commands I hit accidentaly

(put 'list-buffers 'disabled t) ; "C-x C-b", occasionally hit this when trying to "C-x b"
(put 'view-emacs-news 'disabled t) ; "C-h n", occasionally hit when navigating

;;;; Font size

;; Add ability to control emacs-wide text-scale if I find myself
;; having to use a monitor where I can't set operating-system wide
;; scaling factor
(define-globalized-minor-mode
    global-text-scale-mode
    text-scale-mode
    (lambda () (text-scale-mode 1)))

(defun global-text-scale-adjust (inc) (interactive)
       (text-scale-set 1)
       (kill-local-variable 'text-scale-mode-amount)
       (setq-default text-scale-mode-amount (+ text-scale-mode-amount inc))
       (global-text-scale-mode 1))

(global-set-key (kbd "C-M-0")
                '(lambda () (interactive)
                   (global-text-scale-adjust (- text-scale-mode-amount))
                   (global-text-scale-mode -1)))
(global-set-key (kbd "C-M-+")
                '(lambda () (interactive) (global-text-scale-adjust 1)))
(global-set-key (kbd "C-M--")
                '(lambda () (interactive) (global-text-scale-adjust -1)))

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

;;;; Configure built-in packages

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

;; Loudly show trailing whitespace and clean it up on save
(use-package whitespace
  :straight f
  :config
  (setq whitespace-line-column 100
        whitespace-style '(face tabs trailing lines-tail))
  (add-hook 'before-save-hook 'whitespace-cleanup)
  (global-whitespace-mode +1))

;; Have saner regex syntax
(use-package re-builder
  :straight f
  :config
  (setq reb-re-syntax 'string))

;;;; Configure text editing packages

;; TODO: figure out a better keyboard shortcut for move-text
(use-package move-text
  :straight t
  :bind (("M-<up>"   . move-text-up)
         ("M-<down>" . move-text-down)))

(use-package multiple-cursors
  :straight t
  :bind (("C-c m l" . mc/edit-lines)
         ("C-c m c" . mc/mark-all-dwim)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/unmark-next-like-this)))

(use-package expand-region
  :straight t
  :bind ("C-:" . er/expand-region))

;;;; Configure VCS packages

(use-package magit
  :straight t
  :bind (("C-c g g" . magit-status)
         ("C-c g l" . magit-log)
         ("C-c g f" . magit-log-buffer-file)
         ("C-c g b" . magit-blame)))

;;;; Configure autocomplete packages

(use-package ivy
  :straight t
  :bind (("C-x b" . ivy-switch-buffer)
         ("C-x 4 b" . ivy-switch-buffer-other-window))
  :config
  (setq ivy-use-selectable-prompt t)
  (ivy-mode 1))

(use-package swiper
  :straight t
  :bind ("C-s" . swiper))

(use-package counsel
  :straight t
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-c f" . counsel-git)
         ("C-c s g" . counsel-git-grep)
         ("C-c s r" . counsel-rg)))

(use-package company
  :straight t
  :hook (prog-mode . company-mode)
  :config
  (setq company-idle-delay 0.2
        company-show-numbers t
        company-tooltip-limit 10
        company-minimum-prefix-length 2
        company-tooltip-align-annotations t))

;;;; Configure programming packages

(use-package flycheck
  :straight t
  :hook (prog-mode . flycheck-mode))

(use-package smartparens
  :straight t
  :hook (prog-mode . smartparens-mode)
  :config
  (require 'smartparens-config)
  (setq sp-base-key-bindings 'paredit
        sp-autoskip-closing-pair nil
        sp-hybrid-kill-entire-symbol nil)
  (sp-use-paredit-bindings))

(use-package rust-mode
  :straight t
  :mode ("\\.rs\\'" . rust-mode)
  :config
  (setq rust-format-on-save nil)
  (defun setup-rust-mode ()
    (eldoc-mode +1)
    (subword-mode +1))
  (add-hook 'rust-mode-hook 'setup-rust-mode))

(use-package typescript-mode
  :straight t
  :mode ("\\.(js|jsx|ts|tsx)\\'")
  :config
  (defun setup-typescript-mode ()
    (eldoc-mode +1)
    (subword-mode +1))
  (add-hook 'typescript-mode-hook 'setup-typescript-mode))

(use-package glsl-mode
  :straight t
  :mode (("\\.glsl\\'" . glsl-mode)
         ("\\.vert\\'" . glsl-mode)
         ("\\.frag\\'" . glsl-mode)
         ("\\.geom\\'" . glsl-mode))
  :config
  (defun setup-glsl-mode ()
    (eldoc-mode +1)
    (subword-mode +1))
  (add-hook 'glsl-mode-hook 'setup-glsl-mode))

;;;; Configure macOS specific packages

(use-package exec-path-from-shell
  :if (eq system-type 'darwin)
  :straight t
  :config
  (exec-path-from-shell-initialize))

(when (eq system-type 'darwin)
  (setq mac-option-key-is-meta t
        mac-command-key-is-meta nil
        mac-command-modifier 'super
        mac-option-modifier 'meta
        ns-function-modifier 'hyper))

(require 'yanchith)
(global-set-key (kbd "C-a") 'yanchith-move-beginning-of-line)
(global-set-key (kbd "C-j") 'yanchith-top-join-line)
(global-set-key (kbd "C-c d") 'yanchith-duplicate-current-line-or-region)
(global-set-key (kbd "M-p") 'yanchith-move-to-previous-blank-line)
(global-set-key (kbd "M-n") 'yanchith-move-to-next-blank-line)

;; Restore gc threshold for better interactivity and shorter pauses
(setq gc-cons-threshold (megabytes 1))

;; TODO: Spellchecking via Flyspell?
