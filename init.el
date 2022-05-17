;;; init.el --- My humble emacs config

;;; Commentary:

;; This is a `use-package' and `straight.el' based config focused on "minimal"
;; functionality and good startup time. It requires emacs version 27.1 or newer.

;;; Code:

;; Always load newest byte code
(setq load-prefer-newer t)

(defun megabytes (n)
  "Return the number of bytes in N megabytes."
  (* n 1024 1024))

;; Increase GC threshold to 50MB for better throughput during init
(setq gc-cons-threshold (megabytes 50))

;;;; Large file perf mitigations

;; Warn before opening larger files
(setq large-file-warning-threshold (megabytes 10))

;; Disable RTL features
(setq-default bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

;; Automatically mitigate (disable slow major and minor modes) when Emacs
;; suspects long lines in the file.
(setq-default so-long-max-lines 500) ; Search first 500 lines
(setq-default so-long-threshold 500) ; Trigger so-long mode when a line is longer than 500 chars
(global-so-long-mode 1)

;; Define directories

(defvar yan-dir-root (file-name-directory load-file-name)
  "The root dir of this config.")

(defvar yan-dir-lisp (expand-file-name "lisp" yan-dir-root)
  "This directory stores custom Lisp code.")
(add-to-list 'load-path yan-dir-lisp)

(defvar yan-dir-savefile (expand-file-name "savefile" yan-dir-root)
  "This directory stores all automatically generated save/history files.")
(unless (file-exists-p yan-dir-savefile)
  (make-directory yan-dir-savefile))

;; Store config changes made through the customize UI here
(setq custom-file (expand-file-name "custom.el" yan-dir-root))

;;;; Customize UI

;; Make the initial frame a bit wider to make space for line numbers in the
;; gutter and still be at have 80 chars space
(when window-system (set-frame-size (selected-frame) 84 38))

;; Customize actions Emacs tries when displaying a buffer in a "new other
;; window" (e.g. magit, help, or compilation). This is almost the same as the
;; default, except:
;;
;; - It doesn't contain display-buffer--maybe-pop-up-frame-or-window, which
;;   causes it to do horizontal splits (after the doing the first vertical
;;   split) on monitors with larger screen real estate.
;;
;; - It doesn't contain display-buffer-pop-up-frame, which causes it to create
;;   new frames, if there not enough space left in the current frame.
(setq display-buffer-base-action '((display-buffer--maybe-same-window
                                    display-buffer-reuse-window
                                    display-buffer-use-some-window
                                    display-buffer-in-previous-window)))

;;;; Customize keyboard on macOS

(when (eq system-type 'darwin)
  (setq mac-option-key-is-meta t
        mac-command-key-is-meta nil
        mac-command-modifier 'super
        mac-option-modifier 'meta
        ns-function-modifier 'hyper))

;; Set the default font

(if (find-font (font-spec :name "Liberation Mono"))
    (cond ((eq system-type 'darwin) (set-frame-font "Liberation Mono-15"))
          ((eq system-type 'windows-nt) (set-frame-font "Liberation Mono-11"))
          ((eq system-type 'gnu/linux) (set-frame-font "Liberation Mono-11")))
  (cond ((eq system-type 'darwin) (set-frame-font "Monaco-15"))))

(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))


;; Do handmade-ish color theme

(set-foreground-color "burlywood3")
(set-background-color "#202020")
(set-cursor-color "#40FF40")

(set-face-attribute 'mode-line nil :foreground "grey80" :background "grey25")
(set-face-attribute 'mode-line-inactive nil :foreground "grey80" :background "#202020")

(set-face-attribute 'font-lock-comment-face nil :foreground "gray50" :weight 'normal)
(set-face-attribute 'font-lock-doc-face nil :foreground "gray70" :weight 'normal)

(set-face-attribute 'font-lock-string-face nil :foreground "olive drab" :weight 'normal)

;; Highlight things: warnings > builtins > keywords > other.
;; Types could be highlighted as well, but that does look very noisy in
;; languages with a lot of them (e.g. Rust).
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
      initial-scratch-message "")

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

;; Do not word wrap, instead just don't render invisible parts of long lines
(setq-default truncate-lines t)
(defun yan-toggle-truncate-lines ()
  "Toggle truncation of lines."
  (interactive)
  (toggle-truncate-lines))

;; Show line numbers
(global-display-line-numbers-mode t)
;; TODO(yan): This is nice, but it flickers when moving point vertically on
;; Windows (and maybe elsewhere too?).
;;
;; (setq display-line-numbers-type 'relative)

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

;; Always open files with UTF-8 and LF encoding by default

(prefer-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)

;; Give comments more space

(setq-default fill-column 80)

;; Smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

;; Don't use tabs to indent, but set them to appear at 4 spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default tab-stop-list (number-sequence 4 120 4))

;; Change indentation to respect our tab width, but also see
;; yan-set-indent-level functions below
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)
(defvaralias 'sgml-basic-offset 'tab-width)
(defvaralias 'js-indent-level 'tab-width)
(defvaralias 'typescript-indent-level 'tab-width)

(defun yan-set-indent-level-2 ()
  "Set global indentation level to 2."
  (interactive)
  (setq tab-width 2))

(defun yan-set-indent-level-4 ()
  "Set global indentation level to 4."
  (interactive)
  (setq tab-width 4))

;; Newline at end of file
(setq require-final-newline t)

;; Delete the selection with a keypress
(delete-selection-mode t)

;; confirm closing emacs

(setq confirm-kill-emacs 'yes-or-no-p)

;;;; Disable some enabled-by-default commands I hit accidentaly

(global-unset-key (kbd "C-x C-b")) ;; 'list-buffers, hit when trying to "C-x b"
(global-unset-key (kbd "C-x C-n")) ;; 'set-goal-column, hit when navigating
(global-unset-key (kbd "C-x C-l")) ;; 'downcase-region, hit when doing "C-x C-;"
(global-unset-key (kbd "C-x C-u")) ;; 'capitalize-region, symmetrical to the above
(global-unset-key (kbd "C-h C-n")) ;; 'view-emacs-news, hit when navigating
(global-unset-key (kbd "C-h n"))   ;; 'view-emacs-news, hit when navigating
(global-unset-key (kbd "C-t"))     ;; 'transpose-chars, hit when opening a new browser tab

;;;; Font size

;; Add ability to control emacs-wide text-scale for presenting or if I find
;; myself having to use a monitor where I can't set operating-system wide
;; scaling factor

(define-globalized-minor-mode
    global-text-scale-mode
    text-scale-mode
    (lambda () (text-scale-mode 1)))

(defun global-text-scale-adjust (inc)
  (interactive)
  (text-scale-set 1)
  (kill-local-variable 'text-scale-mode-amount)
  (setq-default text-scale-mode-amount (+ text-scale-mode-amount inc))
  (global-text-scale-mode 1))

(defun yan-text-scale-inc ()
  "Increase text scale."
  (interactive)
  (global-text-scale-adjust 1))

(defun yan-text-scale-dec ()
  "Decrese text scale."
  (interactive)
  (global-text-scale-adjust -1))

(defun yan-text-scale-reset ()
  "Reset text scale to default."
  (interactive)
  (global-text-scale-adjust (- text-scale-mode-amount))
  (global-text-scale-mode -1))

(global-set-key (kbd "C-M-0") 'yan-text-scale-reset)
(global-set-key (kbd "C-M-+") 'yan-text-scale-inc)
(global-set-key (kbd "C-M--") 'yan-text-scale-dec)

;;;; Misc customizations

;; TODO(yan): Audit
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

;;;; Configure text editing packages

;; TODO(yan): figure out a better keyboard shortcut for move-text
(use-package move-text
  :straight t
  :bind (("S-M-<up>"   . move-text-up)
         ("S-M-<down>" . move-text-down)))

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
         ("C-c s r" . counsel-rg))
  :config
  ;; Original is "git ls-files -z --full-name --", it implicitle uses --cached.
  ;;
  ;; We add --others and --exclude-standard, which together resolve to untracked
  ;; files. We now have to pass --chached explicitly, because it is no longer
  ;; active if we pass other options.
  (setq counsel-git-cmd "git ls-files -z --cached --others --exclude-standard --full-name --"))

(use-package company
  :straight t
  :hook (prog-mode . company-mode)
  :config
  (setq company-idle-delay 0.2
        company-show-numbers t
        company-tooltip-limit 10
        company-minimum-prefix-length 2
        company-tooltip-align-annotations t
        ;; Don't ever, for any reason, downcase anything to anyone for any
        ;; reason ever, no matter what, no matter where, or who, or who you are
        ;; with, or where you are going, or where you've been... ever, for any
        ;; reason whatsoever.
        company-dabbrev-downcase nil))

;;;; Configure programming packages

(use-package flycheck
  :straight t
  ;; Only enable flycheck for specific setups. For rust, flycheck is too
  ;; expensive, but for typescript it's great.
  :hook (typescript-mode . flycheck-mode))

(use-package smartparens
  :straight t
  :hook (prog-mode . smartparens-mode)
  :config
  (require 'smartparens-config)
  (setq sp-base-key-bindings nil
        sp-autoskip-closing-pair t)
  (sp-use-paredit-bindings))

(use-package rust-mode
  :straight t
  :mode ("\\.rs\\'" . rust-mode)
  :config
  (setq rust-format-on-save nil
        rust-format-show-buffer nil
        rust-format-goto-problem nil)
  (defun setup-rust-mode ()
    (eldoc-mode +1)
    (subword-mode +1))
  (add-hook 'rust-mode-hook 'setup-rust-mode))

(use-package typescript-mode
  :straight tide
  :mode (("\\.js\\'" . typescript-mode)
         ("\\.ts\\'" . typescript-mode)
         ("\\.jsx\\'" . typescript-mode)
         ("\\.tsx\\'" . typescript-mode))
  :config
  (defun setup-typescript-mode ()
    (tide-setup)
    (eldoc-mode +1)
    (subword-mode +1))
  (add-hook 'typescript-mode-hook 'setup-typescript-mode))

(use-package csharp-mode
  :straight t
  :mode ("\\.cs\\'" . csharp-mode)
  :config
  (defun setup-csharp-mode ()
    (subword-mode +1))
  (add-hook 'csharp-mode-hook 'setup-csharp-mode))

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

(require 'wgsl-mode)

(require 'yan)
(global-set-key (kbd "C-a") 'yan-move-beginning-of-line)
(global-set-key (kbd "C-j") 'yan-top-join-line)
(global-set-key (kbd "C-c d") 'yan-duplicate-current-line-or-region)
(global-set-key (kbd "M-p") 'yan-move-to-previous-blank-line)
(global-set-key (kbd "M-n") 'yan-move-to-next-blank-line)

;; Restore gc threshold for better interactivity and shorter pauses
(setq gc-cons-threshold (megabytes 1))

;; TODO(yan): Spellchecking via Flyspell? This is problematic on Windows,
;; because it is tougher to get to ispell or aspell builds.
