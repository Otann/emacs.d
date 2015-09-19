;;; otann-navigation.el --- -*- lexical-binding: t; -*-
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

;;; Project management and switching

(use-package projectile
  :ensure t
  :demand t
  :init (projectile-global-mode)
  :config (progn (run-with-idle-timer 10 nil #'projectile-cleanup-known-projects)

		 (setq projectile-completion-system 'helm
		       projectile-find-dir-includes-top-level t
		       projectile-mode-line '(:eval
					      (let ((text (projectile-project-name)))
						(if (string= text "-")
						    "" (concat " " text " "))))))
  :diminish projectile-mode)

;; (let ((project (projectile-project-name)))
;;						       (cond ((eq project "-") (concat "+++" project)
;;							      (t (concat " " project " ")))))

(use-package ibuffer-projectile         ; Group buffers by Projectile project
  :ensure t
  :defer t
  :init (add-hook 'ibuffer-hook #'ibuffer-projectile-set-filter-groups))

;(use-package window                     ; Standard window functions
;  :bind (("C-c w =" . balance-windows)
;         ("C-c w k" . delete-window)
;         ("C-c w /" . split-window-right)
;         ("C-c w -" . split-window-below)
;         ("C-c w m" . delete-other-windows)))

;(use-package otann-window               ; Personal window utilities
;  :load-path "lisp/"
;  :defer t
;  :bind (("C-c w q" . otann-quit-bottom-side-windows)
;         ("C-c w d" . otann-toggle-current-window-dedication)
;         ("C-c w b" . otann-switch-to-minibuffer-window)))

(use-package window-numbering           ; Fast switching between windows
  :demand t
  :config (progn
	    (window-numbering-mode)
	    ;; Will be included in otann-modeline package manually
	    (window-numbering-clear-mode-line))
  :bind (("C-1" . select-window-1)
	 ("C-2" . select-window-2)
	 ("C-3" . select-window-3)
	 ("C-4" . select-window-4)
	 ("C-5" . select-window-5)))

(use-package neotree                    ; Show files tree
  :ensure t
  :bind (("C-c f t" . neotree-toggle))
  :config (setq neo-theme 'ascii
		neo-window-width 32
		neo-create-file-auto-open t
		neo-banner-message nil
		neo-show-updir-line nil
		neo-mode-line-type 'neotree
		neo-smart-open t
		neo-dont-be-alone t
		neo-persist-show nil
		neo-show-hidden-files t
		neo-auto-indent-point t))

(setq history-length 1000               ; Store more history
      use-dialog-box nil                ; Never use dialogs for minibuffer input
      )

;;; Helm - Smart completion for commands
;; Helm itself (Powerful minibuffer input framework)

(use-package helm
  :ensure t
  :bind (("C-c c b" . helm-resume))
  :init (progn
	  (helm-mode 1)
	  (with-eval-after-load 'helm-config
	    (warn "`helm-config' loaded! Get rid of it ASAP!")))
  :config (progn
	    (setq helm-split-window-in-side-p t
		  helm-display-header-line nil)
	    (set-face-attribute 'helm-source-header nil ; Inherit some style from font-lock
				:foreground (face-foreground 'font-lock-constant-face)
				:background (face-background 'font-lock-constant-face))
	    (set-face-attribute 'helm-match nil
				;:underline (:color foreground-color :style line)
				:foreground (face-foreground 'font-lock-keyword-face)))
  :diminish helm-mode)

(use-package helm-files                 ; Helm for file finding
  :ensure helm
  :defer t
  :bind (([remap find-file] . helm-find-files)
	 ("C-c f r"         . helm-recentf))
  :config (setq helm-recentf-fuzzy-match t
		;; Use recentf to find recent files
		helm-ff-file-name-history-use-recentf t
		;; Find library from `require', `declare-function' and friends
		helm-ff-search-library-in-sexp t))

;; Misc helm commands
(use-package helm-misc
  :ensure helm
  :bind (([remap switch-to-buffer] . helm-mini)))

(use-package helm-command               ; M-x in Helm
  :ensure helm
  :bind (([remap execute-extended-command] . helm-M-x)))

(use-package helm-projectile            ; Helm frontend for Projectile
  :ensure t
  :defer t
  :init (with-eval-after-load 'projectile (helm-projectile-on))
  :config (progn (setq projectile-switch-project-action #'helm-projectile)))

;; Configure `display-buffer' behaviour for some special buffers.
(setq display-buffer-alist
      `(; Put Helm, REPLs and error lists into the bottom side window
	(,(rx bos (or "*Flycheck errors*" ; Flycheck error list
		      "*compilation"      ; Compilation buffers
		      "*Warnings*"        ; Emacs warnings
		      "*sbt"              ; SBT REPL and compilation buffer
		      "*SQL"              ; SQL REPL
		      "*shell"            ; Shell window
		      "*helm"             ; Helm buffers
		      ))
	 (display-buffer-reuse-window
	  display-buffer-in-side-window)
	 (side            . bottom)
	 (reusable-frames . visible)
	 (window-height   . 0.2))
	;; Let `display-buffer' reuse visible frames for all buffers.  This must
	;; be the last entry in `display-buffer-alist', because it overrides any
	;; later entry with more specific actions.
	("." nil (reusable-frames . visible))))

(provide 'otann-navigation)
;;; otann-navigation.el ends here
