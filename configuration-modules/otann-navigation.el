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

;; Group buffers by Projectile project
(use-package ibuffer-projectile
  :ensure t
  :defer t
  :init (add-hook 'ibuffer-hook #'ibuffer-projectile-set-filter-groups))

(use-package dired
  :defer t
  :config (progn
            (require 'dired-x)

            (setq dired-auto-revert-buffer t    ; Revert on re-visiting
                  ;; Better dired flags: `-l' is mandatory, `-a' shows all files, `-h'
                  ;; uses human-readable sizes, and `-F' appends file-type classifiers
                  ;; to file names (for better highlighting)
                  dired-listing-switches "-alhF"
                  dired-ls-F-marks-symlinks t   ; -F marks links with @
                  ;; Inhibit prompts for simple recursive operations
                  dired-recursive-copies 'always
                  ;; Auto-copy to other Dired split window
                  dired-dwim-target t)))

;; Additional tools for Dired
(use-package dired-x
  :bind (("C-x C-j" . dired-jump))
  :init (add-hook 'dired-mode-hook #'dired-omit-mode)
  :config
  (progn
    (setq dired-omit-verbose nil)        ; Shut up, dired

    (when (eq system-type 'darwin)
      ;; OS X bsdtar is mostly compatible with GNU Tar
      (setq dired-guess-shell-gnutar "tar"))

    ;; Diminish dired-omit-mode. We need this hack, because Dired Omit Mode has
    ;; a very peculiar way of registering its lighter explicitly in
    ;; `dired-omit-startup'.  We can't just use `:diminish' because the lighter
    ;; isn't there yet after dired-omit-mode is loaded.
    (add-function :after (symbol-function 'dired-omit-startup)
                  (lambda () (diminish 'dired-omit-mode " ⓞ"))
                  '((name . dired-omit-mode-diminish)))))

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

;; Fast switching between windows
(use-package window-numbering
  :demand t
  :config (progn
            (window-numbering-mode)
            ;; Will be included in otann-modeline package manually
            (window-numbering-clear-mode-line))
  :bind (("C-1" . select-window-1)
         ("C-2" . select-window-2)
         ("C-3" . select-window-3)
         ("C-4" . select-window-4)
         ("C-5" . select-window-5)
         ("C-0" . select-window-0)))

;; Show files tree
(use-package neotree
  :ensure t
  :bind (("C-c f t" . neotree-toggle))
  :config (progn

            (bind-key "r" 'neotree-change-root neotree-mode-map)
            (bind-key "<left>" 'neotree-select-up-node neotree-mode-map)
            (bind-key "<right>" 'neotree-select-down-node neotree-mode-map)
            (setq neo-theme 'ascii
                  neo-window-width 32
                  neo-create-file-auto-open t
                  neo-banner-message nil
                  neo-show-updir-line nil
                  neo-mode-line-type 'neotree
                  neo-smart-open t
                  neo-dont-be-alone t
                  neo-persist-show nil
                  neo-show-hidden-files t
                  neo-auto-indent-point t
                  neo-keymap-style 'concise)))

;; Store more history
; Never use dialogs for minibuffer input
(setq history-length 1000
      use-dialog-box nil)

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

;; M-x in Helm
(use-package helm-command
  :ensure helm
  :bind (([remap execute-extended-command] . helm-M-x)))

;; Helm frontend for Projectile
(use-package helm-projectile
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

;; Trick to swap two windows. From Steve Yegge
;; someday might want to rotate windows if more than 2 of them
;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun swap-windows ()
 "If you have 2 windows, it swaps them."
 (interactive)
 (cond ((not (= (count-windows) 2))
        (message "You need exactly 2 windows to do this."))
 (t
  (let* ((w1 (first (window-list)))
         (w2 (second (window-list)))
         (b1 (window-buffer w1))
         (b2 (window-buffer w2))
         (s1 (window-start w1))
         (s2 (window-start w2)))
    (set-window-buffer w1 b2)
    (set-window-buffer w2 b1)
    (set-window-start w1 s2)
    (set-window-start w2 s1)))))

;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "New name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(provide 'otann-navigation)
;;; otann-navigation.el ends here
