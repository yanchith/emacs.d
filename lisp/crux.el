;;; crux.el --- Helpers, stolen from crux -*- lexical-binding: t; -*-
;;; Code:

(defvar crux-line-start-regex-term-mode "^[^#$%>\n]*[#$%>]")
(defvar crux-line-start-regex-eshell-mode "^[^$\n]*$")
(defvar crux-line-start-regex "^[[:space:]]*")

(defun move-to-mode-line-start ()
  "Move to the beginning, skipping mode specific line start regex."
  (interactive)
  (move-beginning-of-line nil)
  (let ((line-start-regex (cond ((eq major-mode 'term-mode) crux-line-start-regex-term-mode)
                                ((eq major-mode 'eshell-mode) crux-line-start-regex-eshell-mode)
                                (t crux-line-start-regex))))
    (search-forward-regexp line-start-regex (line-end-position) t)))

(defun crux-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (move-to-mode-line-start)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(defun crux-top-join-line ()
  "Join the current line with the line beneath it."
  (interactive)
  (delete-indentation 1))

(defun crux-duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated.  However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (pcase-let* ((origin (point))
               (`(,beg . ,end) (crux-get-positions-of-line-or-region))
               (region (buffer-substring-no-properties beg end)))
    (dotimes (_i arg)
      (goto-char end)
      (newline)
      (insert region)
      (setq end (point)))
    (goto-char (+ origin (* (length region) arg) arg))))

(defun crux-get-positions-of-line-or-region ()
  "Return positions (beg . end) of the current line or region."
  (let (beg end)
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (cons beg end)))

(provide 'crux)
;;; crux.el ends here
