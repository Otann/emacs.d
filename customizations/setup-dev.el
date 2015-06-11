;;; Commentary:
;; This describes commoh helpers for all programming features

;;; Code:

(require 'flycheck)
(setq flycheck-indication-mode 'right-fringe)

;; Prefix all FlyCheck with C-c f
(define-key flycheck-mode-map flycheck-keymap-prefix nil)
(setq flycheck-keymap-prefix (kbd "C-c f"))
(define-key flycheck-mode-map flycheck-keymap-prefix flycheck-command-map)


;(add-hook 'after-init-hook 'global-company-mode)
;(global-set-key (kbd "S-<SPC>") 'company-complete)

; autocomplete configuration
(require 'auto-complete-config)
(setq ac-delay 0.0)
(setq ac-quick-help-delay 0.0)
(ac-config-default)

