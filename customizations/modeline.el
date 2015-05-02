;; This file customizes the way mode line is hown
;; @see http://www.lunaryorn.com/2014/07/26/make-your-emacs-mode-line-more-useful.html 

(setq-default mode-line-position
              '((-3 "%p") (size-indication-mode ("/" (-4 "%I")))
                " "
                (line-number-mode ("%l" (column-number-mode ":%c")))))

;; Custom field for projectile
(defvar otann-projectile-mode-line
  '(:propertize
    (:eval (when (ignore-errors (projectile-project-root))
             (concat " " (projectile-project-name) " ")))
    face match)                                      ;; style for Project modeline
  "Mode line format for Projectile.")
(put 'otann-projectile-mode-line 'risky-local-variable t)

;; Custom window number

(defvar otann-window-number-mode-line
  '(:propertize
    (:eval (concat " " (window-numbering-get-number-string) " "))
    face lazy-highlight)
  "Mode line format for Window number")
(put 'otann-window-number-mode-line 'risky-local-variable t)
 
(setq-default mode-line-format
              '("%e" mode-line-front-space
                otann-window-number-mode-line " "
                mode-line-mule-info              ; '-:'
                mode-line-client                 
                mode-line-modified               ; '**' or '--'
                mode-line-remote                 ; 
                mode-line-frame-identification
                mode-line-buffer-identification " " mode-line-position ; '13% 46' or 'Bot 63'

                " " otann-projectile-mode-line   ; Project information
                
                ;; And the modes, which I don't really care for anyway
                " " mode-line-modes mode-line-end-spaces
                ))

