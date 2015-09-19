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

;; Clean up trailing whitespaces on file save
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Magit & Ediff
(setq magit-last-seen-setup-instructions "1.4.0")
(setq ediff-split-window-function 'split-window-horizontally)

;;; Paired delimiters - Parenthesis editing and balancing
(use-package smartparens
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

;; On-the-fly syntax checking
(use-package flycheck
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

;; Install code-folding
(use-package hideshowvis
  :ensure t
  :config (progn
	    (hideshowvis-symbols)
	    (set-face-attribute 'hs-face nil
				:box nil
				:background (face-foreground 'default)
				:foreground (face-background 'default))))

;;; Autocompletion framework
;; Graphical (auto-)completion
(use-package company
  :ensure t
  :init (global-company-mode)
  :config (setq company-tooltip-align-annotations t
		company-tooltip-flip-when-above t
		;; Easy navigation to candidates with M-<n>
		company-show-numbers t)
  :diminish (company-mode . "ⓒ"))

;; Show help in tooltip
(use-package company-quickhelp
  :ensure t
  :defer t
  :init (with-eval-after-load 'company
	  (company-quickhelp-mode)))

;; Sort company candidates by statistics
(use-package company-statistics
  :ensure t
  :defer t
  :init (with-eval-after-load 'company
	  (company-statistics-mode)))

;; Completion for Math symbols
(use-package company-math
  :ensure t
  :defer t
  :init (with-eval-after-load 'company
	  ;; Add backends for math characters
	  (add-to-list 'company-backends 'company-math-symbols-unicode)
	  (add-to-list 'company-backends 'company-math-symbols-latex)))

;; Helm frontend for company
(use-package helm-company
  :ensure t
  :defer t
  :init (with-eval-after-load 'company
	  ;; Use Company for completion
	  (bind-key [remap completion-at-point] #'helm-company company-mode-map)
	  (bind-key "C-:" #'helm-company company-mode-map)
	  (bind-key "C-:" #'helm-company company-active-map)))

(use-package diff-hl
  :ensure t
  :defer t
  :init (progn
	  ;; Highlight changes to the current file in the fringe
	  (global-diff-hl-mode)
	  ;; Highlight changed files in the fringe of Dired
	  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)

	  ;; Fall back to the display margin, if the fringe is unavailable
	  (unless (display-graphic-p)
	    (diff-hl-margin-mode))))

;;; Git stuff
;; Git configuration mode
(use-package gitconfig-mode
  :ensure t
  :defer t)

;; .gitignore mode
(use-package gitignore-mode
  :ensure t
  :defer t)

;; Go back in Git time
(use-package git-timemachine
  :ensure t
  :bind (("C-c v t" . git-timemachine)))

(provide 'otann-ide)
;;; otann-ide.el ends here
