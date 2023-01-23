(deftheme abu
  "just a theme")

;; Add font-lock-number-face, so we can target it and highlight numbers
(require 'font-lock)
(defun yan-copy-face (new-face face)
  "Define NEW-FACE from existing FACE."
  (copy-face face new-face)
  (eval `(defvar ,new-face nil))
  (set new-face new-face))

(yan-copy-face 'font-lock-number-face 'font-lock-string-face)

(custom-theme-set-faces
 'abu
 ;; Frame

 `(default ((t (:foreground "burlywood3" :background "#191919"))))
 `(cursor  ((t (:background "#50ffa0"))))
 `(region  ((t (:background "#202522"))))

 `(minibuffer-prompt ((t (:foreground "#f06525"))))

 `(fringe ((t (:foreground nil :background nil))))

 `(modeline ((t (:background "gray80" :foreground "gray25"))))
 `(modeline-inactive ((t (:background "gray80" :foreground "#202020"))))

 `(show-paren-match-face ((t (:background "#536058"))))

 ;; Main

 ;; Highlight things: warnings > builtins > keywords > other.
 ;; Types could be highlighted as well, but that does look very noisy in
 ;; languages with a lot of them (e.g. Rust).
 `(font-lock-warning-face       ((t (:foreground "#f06525" :bold t))))
 `(font-lock-builtin-face       ((t (:foreground "#e95410"))))
 `(font-lock-keyword-face       ((t (:foreground "#e95410"))))
 `(font-lock-comment-face       ((t (:foreground "#7d7d7d"))))
 `(font-lock-type-face          ((t (:foreground "#burlywood3"))))
 `(font-lock-constant-face      ((t (:foreground "#burlywood3"))))
 `(font-lock-function-name-face ((t (:foreground "#burlywood3"))))
 `(font-lock-variable-name-face ((t (:foreground "#burlywood3"))))

 `(font-lock-string-face ((t (:foreground "#98c379"))))
 `(font-lock-number-face ((t (:foreground "#759fbf"))))

 `(font-lock-comment-face ((t (:foreground "#gray50"))))
 `(font-lock-doc-face     ((t (:foreground "#gray70"))))

 ;; rainbow-delimiters
 `(rainbow-delimiters-depth-1-face ((t (:foreground "#990000"))))
 `(rainbow-delimiters-depth-2-face ((t (:foreground "#383838"))))
 `(rainbow-delimiters-depth-3-face ((t (:foreground "#505050"))))
 `(rainbow-delimiters-depth-4-face ((t (:foreground "#6c6c6c"))))
 `(rainbow-delimiters-depth-5-face ((t (:foreground "#888888"))))
 `(rainbow-delimiters-depth-6-face ((t (:foreground "#a7a7a7"))))
 `(rainbow-delimiters-depth-7-face ((t (:foreground "#c4c4c4"))))
 `(rainbow-delimiters-depth-8-face ((t (:foreground "#e1e1e1"))))
 `(rainbow-delimiters-depth-9-face ((t (:foreground "#ffffff"))))

 ;; swiper and ivy
 `(swiper-line-face ((t (:background nil))))

 `(swiper-background-match-face-1 ((t (:background "#343c37"))))
 `(swiper-background-match-face-2 ((t (:background "#3e4842"))))
 `(swiper-background-match-face-3 ((t (:background "#48544d"))))
 `(swiper-background-match-face-4 ((t (:background "#536058"))))

 ;; TODO(yan): If I set these, they for some reason multiply with the background
 ;; faces, making the match unreadable.
 ;;
 ;; `(swiper-match-face-1 ((t (:background "#343c37"))))
 ;; `(swiper-match-face-2 ((t (:background "#3e4842"))))
 ;; `(swiper-match-face-3 ((t (:background "#48544d"))))
 ;; `(swiper-match-face-4 ((t (:background "#536058"))))

 `(ivy-minibuffer-match-face-1 ((t (:background "#343c37"))))
 `(ivy-minibuffer-match-face-2 ((t (:background "#3e4842"))))
 `(ivy-minibuffer-match-face-3 ((t (:background "#48544d"))))
 `(ivy-minibuffer-match-face-4 ((t (:background "#536058")))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'abu)
