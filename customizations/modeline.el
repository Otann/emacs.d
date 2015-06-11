;;; Package --- Summary
;;; Commentary:
;; This file customizes the way mode line is hown
;; @see http://www.lunaryorn.com/2014/07/26/make-your-emacs-mode-line-more-useful.html 

;;; Code:

;; Abbreviate or hide minor modes names
(require 'delight)
(delight '((emacs-lisp-mode "ELisp " lisp-mode)
           (js-mode "JS " js)
           (js2-mode "JS2 " js2-mode)
           (web-mode "Web " web-mode)
           (gfm-mode "MD " markdown-mode)
           (clojure-mode "λλ " clojure-mode)
           (js2-refactor-mode "Ⓡ" js2-refactor)
           (paredit-mode "ⓟ" paredit)
           (flycheck-mode "ⓕ" flycheck)
           (subword-mode "ⓢ" subword)
           (company-mode "ⓒ" company)
           (auto-complete-mode "ⓒ" auto-complete)
           (projectile-mode nil projectile)
           (guide-key-mode nil guide-key)
           (eldoc-mode nil "eldoc")
           (visual-line-mode nil simple)
           (yas-minor-mode nil yasnippet)
           (wakatime-mode)
           ))

;; Customise display of position in file
(setq line-number-mode t)
(setq column-number-mode t)
(setq-default mode-line-position
              '(;(-3 "%p") ; Top / Bot / 34%
                ;(size-indication-mode ("/" (-4 "%I")))
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

(setq flycheck-mode-line
      '(:eval
        (pcase flycheck-last-status-change
          (`not-checked nil)
          (`no-checker (propertize " - " 'face 'flycheck-fringe-warning))
          (`running (propertize " ✷ " 'face 'mode-line))
          (`errored (propertize " ! " 'face 'flycheck-fringe-error))
          (`finished
           (let* ((error-counts (flycheck-count-errors flycheck-current-errors))
                  (no-errors (cdr (assq 'error error-counts)))
                  (no-warnings (cdr (assq 'warning error-counts)))
                  (face (cond (no-errors 'flycheck-fringe-error)
                              (no-warnings 'flycheck-fringe-warning)
                              (t 'mode-line))))
             (propertize (format " %s/%s " (or no-errors 0) (or no-warnings 0))
                         'face face)))
          (`interrupted " - ")
          (`suspicious '(propertize " ? " 'face 'flycheck-fringe-warning)))))

(defvar otann-vc-mode-line
  '(" " (:propertize
         ;; Strip the backend name from the VC status information
         (:eval (let ((backend (symbol-name (vc-backend (buffer-file-name)))))
                  (substring vc-mode (+ (length backend) 2))))
         face modeline))
  "Mode line format for VC Mode.")
(put 'otann-vc-mode-line 'risky-local-variable t)
 
(setq-default mode-line-format
              '("%e" 
                otann-window-number-mode-line " "
                ; mode-line-client                 
                mode-line-modified " "                 ; '**' or '--'
                ; mode-line-remote                     ; for remote ssh connections 
                ; mode-line-frame-identification       ; for multiframe environment
                mode-line-buffer-identification " " 

                otann-projectile-mode-line             ; Project information
                (vc-mode otann-vc-mode-line) " "       ; Branch information

                (flycheck-mode flycheck-mode-line) " " ; Flycheck status
                mode-line-position " "                 ; line number
                
                ;; And the modes, which I don't really care for anyway
                mode-line-modes mode-line-end-spaces
                ))

