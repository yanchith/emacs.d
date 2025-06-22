;; This is a `use-package' and `straight.el' based config focused on "minimal"
;; functionality and good startup time. The config targets the Emacs version I
;; use, which is usually the latest stable, unless I forget to update.

;; Always load newest byte code
(setq load-prefer-newer t)

(defun megabytes (n)
  "Return the number of bytes in N megabytes."
  (* n 1024 1024))

;; @Perf Increase GC threshold to 50MB for better throughput during init
(setq gc-cons-threshold (megabytes 50))

;; To prevent a flash of white on startup, set the same color as theme would,
;; but earlier. However, since the config may still fail, also set the
;; foreground color to our theme's, so that we at least get visible text (the default text is
;; black), if it does fail.
(set-background-color "#191919")
(set-foreground-color "burlywood")

;;;; Large file perf mitigations

;; Warn before opening larger files
(setq large-file-warning-threshold (megabytes 10))

;; Disable RTL features
(setq-default bidi-paragraph-direction 'left-to-right)
(setq-default bidi-inhibit-bpa t)

;; Automatically mitigate (disable slow major and minor modes) when Emacs
;; suspects long lines in the file.
(setq-default so-long-max-lines 1000) ; Search first 1000 lines
(setq-default so-long-threshold 1000) ; Trigger so-long mode when a line is longer than 1000 chars
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

;; Add a bit of fringe padding and make it apply to all windows
(setq-default left-fringe-width 20
              right-fringe-width 20)
(set-window-buffer nil (current-buffer))

;; @Perf The emacs default for initial-major-mode is lisp-interaction-mode,
;; which is a programming mode. Everything :hook-ed with prog-mode (such as the
;; very expensive flycheck) would be loaded right from the start, if we kept it
;; that way.
(setq initial-major-mode 'fundamental-mode
      initial-scratch-message "")

;; Disable cursor blinking
(blink-cursor-mode -1)

;; Don't highlight the current line
(global-hl-line-mode -1)

;; Configure parentheses behavior

;; @Perf Don't blink matching parenthesis by default as it has a painfully slow
;; interaction with multiple cursors when inserting closing
;; parenthesis. Instead, we highlight the matching paren with show-paren-mode.
(setq blink-matching-paren nil)
(setq-default show-paren-delay 0)
(show-paren-mode)

;; Enable electric-pair-mode to balance all parentheses
(electric-pair-mode)

;; Do not word wrap, instead just don't render invisible parts of long lines
(setq-default truncate-lines t)
(defun yan-toggle-truncate-lines ()
  "Toggle truncation of lines."
  (interactive)
  (toggle-truncate-lines))

(defun yan-toggle-word-wrap ()
  "Toggle word wrap."
  (interactive)
  (toggle-word-wrap))

;; Show line and column numbers, and file size indication in the
;; modeline
(line-number-mode t)
(column-number-mode t)
;; (size-indication-mode t)

;; Display less things in the modeline

(defvar clean-mode-line-alist
  `((ivy-mode . "")
    (subword-mode . "")
    (eldoc-mode . "")))

(defun clean-mode-line ()
  (dolist (cleaner clean-mode-line-alist)
    (let* ((mode (car cleaner))
           (mode-str (cdr cleaner))
           (old-mode-str (cdr (assq mode minor-mode-alist))))
      (when old-mode-str
        (setcar old-mode-str mode-str))
      (when (eq mode major-mode)
        (setq mode-name mode-str)))))

(add-hook 'after-change-major-mode-hook 'clean-mode-line)

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

;;;; Customize editor behavior

;; Always open files with UTF-8 and LF encoding by default

(prefer-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)

;; Give comments more space

(setq-default fill-column 100)

;; Smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

;; Don't use tabs to indent, but set them to appear at 4 spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Change indentation to respect our tab width, but also see
;; yan-set-indent-level functions below
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)
(defvaralias 'sgml-basic-offset 'tab-width)
(defvaralias 'js-indent-level 'tab-width)
(defvaralias 'typescript-indent-level 'tab-width)
(defvaralias 'lua-indent-level 'tab-width)

(defun yan-set-indent-level-1 ()
  "Set global indentation level to 1."
  (interactive)
  (setq tab-width 1))

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

;;;; Disable some enabled-by-default commands

;; I hit these accidentaly and don't want them

(global-unset-key (kbd "C-x C-b")) ;; 'list-buffers, hit when trying to "C-x b"
(global-unset-key (kbd "C-x C-n")) ;; 'set-goal-column, hit when navigating
(global-unset-key (kbd "C-x C-l")) ;; 'downcase-region, hit when doing "C-x C-;"
(global-unset-key (kbd "C-x C-u")) ;; 'capitalize-region, symmetrical to the above
(global-unset-key (kbd "C-h C-n")) ;; 'view-emacs-news, hit when navigating
(global-unset-key (kbd "C-h n"))   ;; 'view-emacs-news, hit when navigating
(global-unset-key (kbd "C-t"))     ;; 'transpose-chars, hit when opening a new browser tab
(global-unset-key (kbd "C-x m"))   ;; 'compose-mail, hit when opening going for C-c m l

;; I replaced these with something else

(global-unset-key (kbd "C-M-p")) ;; Some balanced paren thing, replaced with move-text
(global-unset-key (kbd "C-M-n")) ;; Some balanced paren thing, replaced with move-text

;;;; Font size

;; Add ability to control emacs-wide text-scale for presenting or if I find
;; myself having to use a monitor where I can't set operating-system wide
;; scaling factor

(define-globalized-minor-mode
    global-text-scale-mode
    text-scale-mode
    (lambda () (text-scale-mode 1)))

(defun global-text-scale-adjust (inc)
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

;; Store all backup and autosave files in the tmp dir. This appears to have no
;; effect sometimes (a major mode does it for us?), but emacs by default creates
;; name.txt~ backup files in the same directory, which we do not want.
(setq backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Revert buffers automatically when underlying files are changed externally
;; This still prompts for confirmation if buffer has unsaved changes
(global-auto-revert-mode t)

;; TODO(jt): Make stopping at underscores for move-forward-word work for JAI and GLSL.
(global-subword-mode 1)

;;;; Bootstrap straight.el

;; @Perf This produces a huge (2.5x) init time perf boost by not using find(1)
;; on startup to detect whether a package needs rebuilding at init time.
(setq straight-check-for-modifications '(check-on-save find-when-checking))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;;; Bootstrap use-package

(straight-use-package 'use-package)
(eval-when-compile (require 'use-package))

;;;; Configure ubiquitous packages

;; Make unique and more meaningful names for buffers with the same name
(use-package uniquify
  :straight f
  :config
  (setq uniquify-buffer-name-style 'forward
        uniquify-separator "/"
        uniquify-after-kill-buffer-p t
        uniquify-ignore-buffers-re "^\\*"))

;; Clean up whitespace on save, but don't visualize it
(use-package whitespace
  :straight f
  :config
  (add-hook 'before-save-hook 'whitespace-cleanup))

(use-package move-text
  :straight t
  :bind (("C-M-p" . move-text-up)
         ("C-M-n" . move-text-down)))

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
         ("C-c g b" . magit-blame))
  :config
  ;; @Perf This won't refresh magit's status buffer unless it is the current
  ;; buffer, speeding up some workflows.
  ;;
  ;; (setq magit-refresh-status-buffer nil)

  ;; @Perf These are the slower parts of refreshing the status buffer, but they
  ;; look useful. Can we live without?
  ;;
  ;; (remove-hook 'magit-status-sections-hook 'magit-insert-tags-header)
  ;; (remove-hook 'magit-status-sections-hook 'magit-insert-status-headers)
  ;; (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-pushremote)
  ;; (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-upstream-or-recent)

  ;; @Perf Showing a diff when commiting is slow, and we've already seen the
  ;; diff in the status buffer anyway.
  (remove-hook 'server-switch-hook 'magit-commit-diff)
  (remove-hook 'with-editor-filter-visit-hook 'magit-commit-diff))

;;;; Configure completion packages

(use-package ivy
  :straight t
  ;; TODO(yan): @Perf This costs about 40ms of startup.
  :demand t ;; We pretty much need ivy the moment we start
  :bind (("C-x b"   . ivy-switch-buffer)
         ("C-x 4 b" . ivy-switch-buffer-other-window))
  :config
  (setq ivy-use-selectable-prompt t)
  (ivy-mode 1))

(use-package swiper
  :straight t
  :bind ("C-s" . swiper))

(use-package counsel
  :straight t
  :bind (("M-x"     . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-c s g" . counsel-git-grep)
         ("C-c s r" . counsel-rg)))

;;;; Configure programming packages

;; TODO(yan): @Cleanup Emacs 29 is getting a rust-ts-mode. Maybe it will be good enough for us, if
;; we add a shortcut for formatting and teach it about rustc's error output. We just need to build
;; the treesitter grammars for all platforms we use.
(use-package rust-mode
  :straight t
  :mode ("\\.rs\\'" . rust-mode)
  :config
  (setq rust-format-on-save nil
        rust-format-show-buffer nil
        rust-format-goto-problem nil)
  (defun setup-rust-mode ()
    ;; Unset wrapping with dbg! macro from the keymap.
    (local-unset-key (kbd "C-c C-d"))
    (font-lock-add-keywords
     nil '(
           ; Hex integer
           ("\\<0[Xx][0-9A-Fa-f_]+\\([ui]\\(8\\|16\\|32\\|64\\|128\\|size\\)\\)?\\>" . font-lock-number-face)
           ; Octal integer
           ("\\<0[Oo][0-7_]+\\([ui]\\(8\\|16\\|32\\|64\\|128\\|size\\)\\)?\\>" . font-lock-number-face)
           ; Binary integer
           ("\\<0[Bb][01_]+\\([ui]\\(8\\|16\\|32\\|64\\|128\\|size\\)\\)?\\>" . font-lock-number-face)
           ; Floating point number (integer + comma + fractional + optional suffix)
           ("\\<[0-9_]+\\.[0-9_]+\\([Ee][+-][0-9]+\\)?\\(f\\(32\\|64\\)\\)?\\>" . font-lock-number-face)
           ; Floating point number (integer + comma + no suffix)
           ("\\<[0-9_]+\\.\\>" . font-lock-number-face)
           ; Floating point number (integer + scientific + optional suffix)
           ("\\<[0-9_]+[Ee][-+][0-9]+\\(f\\(32\\|64\\)\\)?\\>" . font-lock-number-face)
           ; Floating point number (integer + suffix)
           ("\\<[0-9_]+f\\(32\\|64\\)\\>" . font-lock-number-face)
           ; Decimal integer
           ("\\<[0-9_]+\\([ui]\\(8\\|16\\|32\\|64\\|128\\|size\\)\\)?\\>" . font-lock-number-face))))
  (add-hook 'rust-mode-hook 'setup-rust-mode))

(use-package glsl-mode
  :straight t
  :mode (("\\.glsl\\'" . glsl-mode)
         ("\\.vert\\'" . glsl-mode)
         ("\\.frag\\'" . glsl-mode)
         ("\\.geom\\'" . glsl-mode))
  :config
  (defun setup-glsl-mode ()
    (modify-syntax-entry ?_ "." glsl-mode-syntax-table)) ;; Let forward-word/backword-word stop on underscores.
  (add-hook 'glsl-mode-hook 'setup-glsl-mode))

(use-package wgsl-mode
  :straight t
  :mode ("\\.wgsl\\'" . wgsl-mode))

(use-package jai-mode
  :straight f
  :mode ("\\.jai\\'" . jai-mode)
  :config
  (defun setup-jai-mode ()
    ;; Override default, so we both get the default comment style we want, and
    ;; also make M-q (fill-paragraph) work well with the usual, C-style,
    ;; comments.
    (setq comment-start "// "
          comment-end ""))
  (add-hook 'jai-mode-hook 'setup-jai-mode))

(use-package lua-mode
  :straight t
  :mode (("\\.lua\\'" . lua-mode)
         ("\\.p8\\'"  . lua-mode)))

;;;; Configure macOS specific packages

(use-package exec-path-from-shell
  :if (eq system-type 'darwin)
  :straight t
  :config
  (exec-path-from-shell-initialize))

;;;; Define and bind a few navigation and editing functions

(defun yan-move-to-previous-blank-line ()
  "Move to the previous line containing nothing but whitespace."
  (interactive)
  (if (not (search-backward-regexp "^[ \t]*\n" nil t))
      (forward-line -1)))

(defun yan-move-to-next-blank-line ()
  "Move to the next line containing nothing but whitespace."
  (interactive)
  (forward-line)
  (if (search-forward-regexp "^[ \t]*\n" nil t)
      (forward-line -1)))

(defun yan-move-beginning-of-line ()
  "Move cursor to beginning of text in the current line, or to
beginning of the current line, if already at beginning of text"
  (interactive)
  (let ((orig-point (point)))
    (move-beginning-of-line nil)
    (search-forward-regexp "^[[:space:]]*" (line-end-position) t)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(defun yan-top-join-line ()
  "Join the current line with the line beneath it."
  (interactive)
  (delete-indentation 1))

(global-set-key (kbd "C-a") 'yan-move-beginning-of-line)
(global-set-key (kbd "M-p") 'yan-move-to-previous-blank-line)
(global-set-key (kbd "M-n") 'yan-move-to-next-blank-line)
(global-set-key (kbd "C-j") 'yan-top-join-line)

;;;; Attempt at a color theme

(require 'abu-theme)
(load-theme 'abu t)

;; @Perf Restore gc threshold for better interactivity and shorter pauses
(setq gc-cons-threshold (megabytes 1))
