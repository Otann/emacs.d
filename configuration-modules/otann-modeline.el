;;; otann-modeline.el --- -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2012-2015 Anton Chebotaev <anton.chebotaev@gmail.com>
;;
;; Author: Anton Chebotaev <anton.chebotaev@gmail.com>
;; URL: https://gihub.com/otann/.emacs.d
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

(require 'use-package)
(require 'flycheck)
(require 'dash)

;; Customise display of position in file
(setq line-number-mode t)
(setq column-number-mode t)
(setq-default mode-line-position
              '((line-number-mode ("%l" (column-number-mode ":%c")))))

;; (defface otann-modeline
;;   '((t (:inherit default)))
;;   "Basic face for highlighting."
;;   :group 'modeline)
;; (let ((background (face-foreground 'font-lock-negation-char-face))
;;       (foreground (face-background 'default)))
;;   (set-face-attribute 'otann-modeline nil
;; 		      :foreground foreground
;; 		      :background background
;; 		      :overline background
;; 		      :box (list :line-width 1 :color background :style nil)))

;; Create variable with window number for later use
(defvar otann-window-number-mode-line
  '(" " (:eval (window-numbering-get-number-string)))
  "Mode line format for Window number.")
(put 'otann-window-number-mode-line 'risky-local-variable t)

;; Strip the backend name from the VC status information
(defvar otann-vc-mode-line
  '((:eval (let ((backend (symbol-name (vc-backend (buffer-file-name)))))
		 (substring vc-mode (+ (length backend) 2)))))
  "Mode line format for VC Mode.")
(put 'otann-vc-mode-line 'risky-local-variable t)

;; Define how to show FlyCheck results
(defun otann-flycheck-mode-line-status ()
  "Create a mode line status text for Flycheck."
  (let* ((menu (mouse-menu-non-singleton flycheck-mode-menu-map))
         (map (make-mode-line-mouse-map
	       'mouse-1
	       (lambda () (interactive) (popup-menu menu))))
         (text-and-face
          (pcase flycheck-last-status-change
            (`not-checked nil)
            (`no-checker '("?" . border))
            (`running    '("*" . border))
            (`errored    '("!" . border))
            (`finished
             (let* ((error-counts (flycheck-count-errors flycheck-current-errors))
                    (no-errors    (cdr (assq 'error error-counts)))
                    (no-warnings  (cdr (assq 'warning error-counts))))
               (cond
                (no-errors
                 (cons (format " %s/%s " no-errors (or no-warnings 0))
		       'flycheck-fringe-error))
		
                (no-warnings
                 (cons (format " %s " no-warnings)
		       'flycheck-fringe-warning))
		
                (t (cons "k" 'border)))))
	    
            (`interrupted (cons "x" nil))
            (`suspicious '("?" . warning)))))
    (when text-and-face
      (propertize (car text-and-face) 'face (cdr text-and-face)
                  'mouse-face 'mode-line-highlight
                  'local-map map))))
(setq flycheck-mode-line '(:eval (otann-flycheck-mode-line-status)))

;;; Final configuration
(setq-default mode-line-format
	      '("%e"
		(:propertize otann-window-number-mode-line
			     face bold)
		" "
		; mode-line-mule-info
                mode-line-client
                mode-line-modified
		" "
                ; mode-line-remote
                ; mode-line-frame-identification
                mode-line-buffer-identification
		" "
		mode-line-position
		
		(:propertize (projectile-mode projectile-mode-line)
			     face bold)
		(:propertize (vc-mode otann-vc-mode-line)
			     face italic)
		" "
		(flycheck-mode flycheck-mode-line) ; Flycheck status
		" "
		mode-line-misc-info
		mode-line-modes
		mode-line-end-spaces))

(provide 'otann-modeline)
;;; otann-modeline.el ends here
