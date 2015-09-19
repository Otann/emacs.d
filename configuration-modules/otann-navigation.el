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
  :config (progn (run-with-idle-timer 10
				      nil
				      #'projectile-cleanup-known-projects))
  :diminish projectile-mode)

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
;  :bind (("C-c w q" . lunaryorn-quit-bottom-side-windows)
;         ("C-c w d" . lunaryorn-toggle-current-window-dedication)
;         ("C-c w b" . lunaryorn-switch-to-minibuffer-window)))

(use-package window-numbering           ; Fast switching between windows
  :demand t
  :config (window-numbering-mode)
  :bind (("C-1" . select-window-1)
	 ("C-2" . select-window-2)
	 ("C-3" . select-window-3)
	 ("C-4" . select-window-4)
	 ("C-5" . select-window-5)))

(use-package uniquify                   ; Make buffer names unique
  :config (setq uniquify-buffer-name-style 'forward))

;;; Minibuffer and Helm

(setq history-length 1000               ; Store more history
      use-dialog-box nil                ; Never use dialogs for minibuffer input
      )

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
	    (set-face-attribute 'helm-selection nil
				:underline nil
				:foreground (face-foreground 'font-lock-keyword-face)
				;:background (face-background 'font-lock-keyword-face)
				))
  :diminish helm-mode)

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
      `(
        ;; Put REPLs and error lists into the bottom side window
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
         (window-height   . 0.33))
        ;; Let `display-buffer' reuse visible frames for all buffers.  This must
        ;; be the last entry in `display-buffer-alist', because it overrides any
        ;; later entry with more specific actions.
        ("." nil (reusable-frames . visible))))

(provide 'otann-navigation)
;;; otann-navigation.el ends here
