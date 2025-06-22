(deftheme abu
  "just a phase")

(custom-theme-set-faces
 'abu
 ;;;; Frame

 `(default         ((t (:foreground "burlywood3" :background "#191919"))))
 `(vertical-border ((t (:foreground "#090909"    :background "#191919"))))

 `(cursor  ((t (:background "#50ffa0"))))
 `(region  ((t (:background "#343c37"))))

 `(minibuffer-prompt ((t (:foreground "#f06525"))))

 `(fringe ((t (:foreground nil :background nil))))

 `(mode-line          ((t (:background "#090909" :foreground "#a0a0a0"))))
 `(mode-line-inactive ((t (:background "#131313" :foreground "#a0a0a0"))))

 `(show-paren-match-face ((t (:background "#536058"))))

 ;;;; Code

 ;; Highlight things: warnings > preprocessor > keywords > builtins > other.
 ;; Types could be highlighted as well, but that does look very noisy in
 ;; languages with a lot of them (e.g. Rust).
 `(font-lock-warning-face       ((t (:foreground "#f06525" :bold t))))
 `(font-lock-preprocessor-face  ((t (:foreground "#e95410" :bold t))))
 `(font-lock-keyword-face       ((t (:foreground "#e95410"))))
 `(font-lock-builtin-face       ((t (:foreground "#e95410"))))
 `(font-lock-type-face          ((t (:foreground "burlywood3"))))
 `(font-lock-constant-face      ((t (:foreground "burlywood3"))))
 `(font-lock-function-name-face ((t (:foreground "burlywood3"))))
 `(font-lock-variable-name-face ((t (:foreground "burlywood3"))))

 `(font-lock-string-face ((t (:foreground "#98c379"))))
 `(font-lock-number-face ((t (:foreground "#759fbf"))))

 `(font-lock-comment-face        ((t (:foreground "gray50"))))
 `(font-lock-doc-face            ((t (:foreground "gray70"))))

 ;;;; mc
 `(mc/cursor-face ((t (:background "#5d6b63"))))

 ;;;; ivy and swiper

 ;; Customizing these is kind of difficult, because they multiply colors...

 ;; TODO(yan): This somehow multiplies with ivy-minibuffer-match-face-{1,2,3,4},
 ;; which is not very pleasant. Also, it overrides the foreground of the
 ;; selected line. This is overall not worth customizing, so we stay with the
 ;; isearch palette here.
 ;;
 ;; `(ivy-current-match ((t (:foreground "burlywood3" :background "#202522"))))

 `(ivy-minibuffer-match-face-1 ((t (:background "#343c37"))))
 `(ivy-minibuffer-match-face-2 ((t (:background "#3e4842"))))
 `(ivy-minibuffer-match-face-3 ((t (:background "#48544d"))))
 `(ivy-minibuffer-match-face-4 ((t (:background "#536058"))))


 `(swiper-line-face ((t (:background nil)))) ;; This was too aggressive

 ;; TODO(yan): If I set these, they for some reason multiply with the background
 ;; faces, making the match unreadable. Staying with the isearch palette for
 ;; now.
 ;;
 ;; `(swiper-match-face-1 ((t (:background "#343c37"))))
 ;; `(swiper-match-face-2 ((t (:background "#3e4842"))))
 ;; `(swiper-match-face-3 ((t (:background "#48544d"))))
 ;; `(swiper-match-face-4 ((t (:background "#536058"))))

 `(swiper-background-match-face-1 ((t (:background "#343c37"))))
 `(swiper-background-match-face-2 ((t (:background "#3e4842"))))
 `(swiper-background-match-face-3 ((t (:background "#48544d"))))
 `(swiper-background-match-face-4 ((t (:background "#536058"))))

 )

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'abu)
