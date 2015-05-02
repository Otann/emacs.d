;; load ergomacs mode
;(require 'ergoemacs-mode)
;(setq ergoemacs-theme "lvl3")
;(setq ergoemacs-keyboard-layout "us") ;; Assumes QWERTY keyboard layout
;(ergoemacs-mode 1)

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; shell scripts
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)

;; No need for ~ files when editing
(setq create-lockfiles nil)

;; Go straight to scratch buffer on startup
(setq inhibit-startup-message t)

;; Toying around
;(setq frame-title-format "GNU Emacs: %b")
; mode-line-format
; header-line-format
; 
; face mode-line
; face mode-line-inactive

;; Magit & Ediff
(setq magit-last-seen-setup-instructions "1.4.0")
(setq ediff-split-window-function 'split-window-horizontally)
