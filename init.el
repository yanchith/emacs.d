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
  "The root dir of the Emacs Prelude distribution.")

(defvar ya/dir-savefile (expand-file-name "savefile" ya/dir-root)
  "This directory stores all automatically generated save/history files.")

(unless (file-exists-p ya/dir-savefile)
  (make-directory ya/dir-savefile))

;; Increase GC threshold to 50MB and large file limit to 100MB

;; TODO: increase only for init
(setq gc-cons-threshold 50000000)
(setq large-file-warning-threshold 100000000)

;; PERSONAL PRELOAD

;; CORE PACKAGES

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


;; CORE UI

;; CORE EDITOR

;; CORE KEYBINDINGS

;; EMACS CUSTOM FILE

(message "Init phase: custom.el")

;; Store config changes made through the customize UI here
(setq custom-file (expand-file-name "custom.el" ya/dir-root))

;; DONE

(message "Emacs ready!")
