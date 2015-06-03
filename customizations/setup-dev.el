;;; Commentary:
;; This describes commoh helpers for all programming features

;;; Code:

(require 'flycheck)
(setq flycheck-indication-mode 'right-fringe)

(add-hook 'after-init-hook 'global-company-mode)
(global-set-key (kbd "S-<SPC>") 'company-complete)
