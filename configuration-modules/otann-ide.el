;;; otann-ide.el --- -*- lexical-binding: t; -*-
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

;;; Paired delimiters
(use-package smartparens                ; Parenthesis editing and balancing
  :ensure t
  :init (progn (smartparens-global-mode)
               (show-smartparens-global-mode)
	       ;; make mode autoload with certain modes
               (dolist (hook '(inferior-emacs-lisp-mode-hook
			       emacs-lisp-mode-hook))
                       (add-hook hook #'smartparens-strict-mode)))
  :config (progn (setq sp-autoskip-closing-pair 'always
		       ;; Don't kill entire symbol on C-k
		       sp-hybrid-kill-entire-symbol nil)
		 (let ((map smartparens-mode-map))
		   (define-key map (kbd "C-M-f") #'sp-forward-sexp)
		   ;; Movement and navigation
		   (define-key map (kbd "C-M-f") #'sp-forward-sexp)
		   (define-key map (kbd "C-M-b") #'sp-backward-sexp)
		   (define-key map (kbd "C-M-u") #'sp-backward-up-sexp)
		   (define-key map (kbd "C-M-d") #'sp-down-sexp)
		   (define-key map (kbd "C-M-p") #'sp-backward-down-sexp)
		   (define-key map (kbd "C-M-n") #'sp-up-sexp)
		   ;; Deleting and killing
		   (define-key map (kbd "C-M-k") #'sp-kill-sexp)
		   (define-key map (kbd "C-M-w") #'sp-copy-sexp)
		   ;; Depth changing
		   (define-key map (kbd "C-c k S") #'sp-splice-sexp)
		   (define-key map (kbd "C-c k r") #'sp-splice-sexp-killing-around)
		   (define-key map (kbd "C-c k <up>") #'sp-splice-sexp-killing-backward)
		   (define-key map (kbd "C-c k <down>") #'sp-splice-sexp-killing-forward)
		   (define-key map (kbd "C-c k ?") #'sp-convolute-sexp)
		   ;; Barfage & Slurpage
		   (define-key map (kbd "C-)") #'sp-forward-slurp-sexp)
		   (define-key map (kbd "C-<right>") #'sp-forward-slurp-sexp)
		   (define-key map (kbd "C-}") #'sp-forward-barf-sexp)
		   (define-key map (kbd "C-<left>") #'sp-forward-barf-sexp)
		   (define-key map (kbd "C-(") #'sp-backward-slurp-sexp)
		   (define-key map (kbd "C-M-<left>") #'sp-backward-slurp-sexp)
		   (define-key map (kbd "C-{") #'sp-backward-barf-sexp)
		   (define-key map (kbd "C-M-<right>") #'sp-backward-barf-sexp)
		   ;; Miscellaneous commands
		   (define-key map (kbd "C-c k s") #'sp-split-sexp)
		   (define-key map (kbd "C-c k j") #'sp-join-sexp)
		   (define-key map (kbd "C-M-t") #'sp-transpose-sexp)))
  :diminish (smartparens-mode . "ⓟ"))

(use-package flycheck                   ; On-the-fly syntax checking
  :ensure t
  :bind (("C-c e l" . list-flycheck-errors)
         ("C-c e n" . flycheck-next-error)
         ("C-c e p" . flycheck-previous-error)
         ("C-c e c" . flycheck-buffer)
         ("C-c e C" . flycheck-clear)
         ("C-c e f" . flycheck-first-error)
         ("C-c e w" . flycheck-copy-errors-as-kill)
         ("C-c t f" . flycheck-mode))
  :init (global-flycheck-mode)
  :config (progn
            (setq flycheck-indication-mode 'right-fringe
		  flycheck-standard-error-navigation nil
                  flycheck-display-errors-function
                  #'flycheck-display-error-messages-unless-error-list
                  flycheck-scalastylerc "scalastyle_config.xml")

            ;; Use italic face for checker name
            (set-face-attribute 'flycheck-error-list-checker-name nil
                                :inherit 'italic))
  :diminish (flycheck-mode . "Ⓕ"))

;;; WakaTime track time spent in projects
;; custom key supposed to be in custom.el
;; @see https://wakatime.com/help/plugins/emacs
(use-package wakatime-mode
  :ensure t
  :config (global-wakatime-mode)
  :diminish wakatime-mode)

(provide 'otann-ide)
;;; otann-ide.el ends here
