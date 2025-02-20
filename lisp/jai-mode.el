(require 'cl)
(require 'rx)
(require 'js)
(require 'compile)

(defconst jai-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\\ "\\" table)

    (modify-syntax-entry ?'  "." table)
    (modify-syntax-entry ?:  "." table)
    (modify-syntax-entry ?+  "." table)
    (modify-syntax-entry ?-  "." table)
    (modify-syntax-entry ?%  "." table)
    (modify-syntax-entry ?&  "." table)
    (modify-syntax-entry ?|  "." table)
    (modify-syntax-entry ?^  "." table)
    (modify-syntax-entry ?!  "." table)
    (modify-syntax-entry ?$  "/" table)
    (modify-syntax-entry ?=  "." table)
    (modify-syntax-entry ?<  "." table)
    (modify-syntax-entry ?>  "." table)
    (modify-syntax-entry ??  "." table)

    ;; Modify some syntax entries to allow nested block comments
    (modify-syntax-entry ?/ ". 124b" table)
    (modify-syntax-entry ?* ". 23n" table)
    (modify-syntax-entry ?\n "> b" table)
    (modify-syntax-entry ?\^m "> b" table)

    table))

(defconst jai-builtins
  '("cast" "it" "it_index" "type_info" "size_of"))

(defconst jai-keywords
  '("if" "ifx" "else" "then" "while" "for" "switch" "case"
    "struct" "enum" "enum_flags"
    "return" "remove" "continue" "break" "defer"
    "inline" "no_inline"
    "using"))

(defconst jai-literals
  '("null" "true" "false"))

(defconst jai-typenames
  '("int" "s8" "s16" "s32" "s64" "u8" "u16" "u32" "u64"
    "float" "float32" "float64"
    "string" "bool"))

(defun jai-wrap-word-rx (s)
  (concat "\\<" s "\\>"))

(defconst jai-dollar-type-rx (rx (group "$" (or (1+ word) (opt "$")))))
(defconst jai-number-rx
  (rx (and
       symbol-start
       (or (and (+ digit) (opt (and (any "eE") (opt (any "-+")) (+ digit))))
           (and "0" (any "xX") (+ hex-digit)))
       (opt (and (any "_" "A-Z" "a-z") (* (any "_" "A-Z" "a-z" "0-9"))))
       symbol-end)))

(defconst jai-font-lock-defaults
  `(
    ;; Keywords
    (,(regexp-opt jai-keywords 'symbols) . font-lock-keyword-face)

    ;; Builtins
    (,(regexp-opt jai-builtins 'symbols) . font-lock-builtin-face)

    ;; Literals
    (,(regexp-opt jai-literals 'symbols) . font-lock-builtin-face)

    ;; Directives
    ("#[_A-Za-z0-9]+" . font-lock-preprocessor-face)
    ("@[_A-Za-z0-9]+" . font-lock-preprocessor-face)

    ;; Strings
    ("\\\".*\\\"" . font-lock-string-face)

    ;; Numbers
    (,(jai-wrap-word-rx jai-number-rx) . font-lock-number-face)

    ;; Types
    (,(regexp-opt jai-typenames 'symbols) . font-lock-type-face)
    (,jai-dollar-type-rx 1 font-lock-type-face)

    ("---" . font-lock-constant-face)
    ))

(defconst jai--defun-rx "\(.*\).*\{")

(defmacro jai-paren-level ()
  `(car (syntax-ppss)))

(defun jai-line-is-defun ()
  "return t if current line begins a procedure"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let (found)
      (while (and (not (eolp)) (not found))
        (if (looking-at jai--defun-rx)
            (setq found t)
          (forward-char 1)))
      found)))

(defun jai-beginning-of-defun (&optional count)
  "Go to line on which current function starts."
  (interactive)
  (let ((orig-level (jai-paren-level)))
    (while (and
            (not (jai-line-is-defun))
            (not (bobp))
            (> orig-level 0))
      (setq orig-level (jai-paren-level))
      (while (>= (jai-paren-level) orig-level)
        (skip-chars-backward "^{")
        (backward-char))))
  (if (jai-line-is-defun)
      (beginning-of-line)))

(defun jai-end-of-defun ()
  "Go to line on which current function ends."
  (interactive)
  (let ((orig-level (jai-paren-level)))
    (when (> orig-level 0)
      (jai-beginning-of-defun)
      (end-of-line)
      (setq orig-level (jai-paren-level))
      (skip-chars-forward "^}")
      (while (>= (jai-paren-level) orig-level)
        (skip-chars-forward "^}")
        (forward-char)))))

;; NOTE: taken from the scala-indent package and modified for Jai.
;;   Still uses the js-indent-line as a base, which will have to be
;;   replaced when the language is more mature.
(defun jai--indent-on-parentheses ()
  (when (and (= (char-syntax (char-before)) ?\))
             (= (save-excursion (back-to-indentation) (point)) (1- (point))))
    (js-indent-line)))

(defun jai--add-self-insert-hooks ()
  (add-hook 'post-self-insert-hook
            'jai--indent-on-parentheses)
  )

;;;###autoload
(define-derived-mode jai-mode prog-mode "Jai"
 :syntax-table jai-mode-syntax-table
 :group 'jai
 (setq bidi-paragraph-direction 'left-to-right)
 (setq-local require-final-newline mode-require-final-newline)
 (setq-local parse-sexp-ignore-comments t)
 (setq-local comment-start-skip "\\(//+\\|/\\*+\\)\\s *")
 (setq-local comment-start "/*")
 (setq-local comment-end "*/")
 (setq-local indent-line-function 'js-indent-line)
 (setq-local font-lock-defaults '(jai-font-lock-defaults))
 (setq-local beginning-of-defun-function 'jai-beginning-of-defun)
 (setq-local end-of-defun-function 'jai-end-of-defun)

 ;; add indent functionality to some characters
 (jai--add-self-insert-hooks)

 (font-lock-fontify-buffer))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.jai\\'" . jai-mode))

(defconst jai--error-regexp
  "^\\([^ \n:]+.*\.jai\\):\\([0-9]+\\),\\([0-9]+\\):")
(push `(jai ,jai--error-regexp 1 2 3 2) compilation-error-regexp-alist-alist)
(push 'jai compilation-error-regexp-alist)

(provide 'jai-mode)
