;;; Package --- Summary
;;; Commentary:
;; This file customizes the way mode line is hown
;; @see http://www.lunaryorn.com/2014/07/26/make-your-emacs-mode-line-more-useful.html 

;;; Code:

;; Abbreviate or hide minor modes names
(require 'delight)
(delight '((paredit-mode " ✱" paredit)
           (flycheck-mode nil flycheck)
           (projectile-mode " ≣" projectile)
           (company-mode nil company)
           (guide-key-mode nil guide-key)
           (eldoc-mode nil "eldoc")
           (subword-mode " ≃" subword)
           (wakatime-mode)
           (flycheck-mode)))

;(eval-after-load "paredit"    '(diminish 'paredit-mode " (:)"))
;(eval-after-load "flycheck"   '(diminish 'flycheck-mode))
;(eval-after-load "company"    '(diminish 'company-mode))
;(eval-after-load "projectile" '(diminish 'projectile-mode))
;(eval-after-load "guide-key"  '(diminish 'guide-key-mode))


;; Customise display of position in file
(setq line-number-mode t)
(setq column-number-mode t)
(setq-default mode-line-position
              '((-3 "%p") 
                (size-indication-mode ("/" (-4 "%I")))
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

(defvar otann-vc-mode-line
  '(" " (:propertize
         ;; Strip the backend name from the VC status information
         (:eval (let ((backend (symbol-name (vc-backend (buffer-file-name)))))
                  (concat " " (substring vc-mode (+ (length backend) 2)) " ")))
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
                (vc-mode otann-vc-mode-line)           ; Branch information

                (flycheck-mode flycheck-mode-line) " " ; Flycheck status
                mode-line-position " "                 ; line number
                
                ;; And the modes, which I don't really care for anyway
                mode-line-modes mode-line-end-spaces
                ))

