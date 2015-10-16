;;; otann-environment-fixup.el --- -*- lexical-binding: t; -*-
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

;; On OS X, an Emacs instance started from the graphical user
;; interface will have a different environment than a shell in a
;; terminal window, because OS X does not run a shell during the
;; login. Obviously this will lead to unexpected results when
;; calling external utilities like make from Emacs.
;; This library works around this problem by copying important
;; environment variables from the user's shell.
;; https://github.com/purcell/exec-path-from-shell
(use-package exec-path-from-shell
  :ensure t
  :if (and (eq system-type 'darwin) (display-graphic-p))
  :config
  (progn
    (when (string-match-p "/zsh$" (getenv "SHELL"))
      ;; Use a non-interactive login shell.  A login shell, because my
      ;; environment variables are mostly set in `.zprofile'.
      (setq exec-path-from-shell-arguments '("-l")))

    (dolist (var '("EMAIL" "PYTHONPATH" "INFOPATH" "JAVA_OPTS"))
      (add-to-list 'exec-path-from-shell-variables var))

    (exec-path-from-shell-initialize)

    (exec-path-from-shell-copy-envs '("PATH"))

    (setq user-mail-address (getenv "EMAIL"))

    ;; Re-initialize the `Info-directory-list' from $INFOPATH.  Since package.el
    ;; already initializes info, we need to explicitly add the $INFOPATH
    ;; directories to `Info-directory-list'.  We reverse the list of info paths
    ;; to prepend them in proper order subsequently
    (with-eval-after-load 'info
      (dolist (dir (nreverse (parse-colon-path (getenv "INFOPATH"))))
        (when dir
          (add-to-list 'Info-directory-list dir))))))


;;; OS X support
;; for some reason it does not work with emacs-mac port
(use-package ns-win                     ; OS X window support
  :preface (defvar ns-pop-up-frames)
  :defer t
  :if (and (window-system) (eq system-type 'darwin))
  :init (progn
          (bind-key "H-s" 'save-buffer)
          (bind-key "H-v" 'yank)
          (bind-key "H-c" 'kill-ring-save)
          (bind-key "H-x" 'kill-region)
          (bind-key "H-z" 'undo))
  :config (setq ns-pop-up-frames nil            ; Don't pop up new frames from the workspace
                mac-option-modifier 'meta       ; Option is simply the natural Meta
                mac-command-modifier 'hyper     ; Leave hyper for MacOS Shortcuts
                mac-right-command-modifier 'left
                mac-right-option-modifier 'none ; Keep right option for accented input
                ;; Just in case we ever need these keys
                mac-function-modifier 'hyper)
           (message "This is evaluated when `foo' is loaded"))

;; For Some reason code above is not used by emacs-mac port
(when (and (window-system) (eq system-type 'darwin))
  (progn (bind-key "H-a" 'mark-whole-buffer)
         (bind-key "H-t" 'toggle-truncate-lines)
         (bind-key "H-s" 'save-buffer)
         (bind-key "H-v" 'yank)
         (bind-key "H-c" 'kill-ring-save)
         (bind-key "H-x" 'kill-region)
         (bind-key "H-z" 'undo)
         (bind-key "H-w" (lambda () (interactive) (kill-buffer)))
         (bind-key "H-q" 'save-buffers-kill-terminal)
         (bind-key "H-<right>" 'move-end-of-line)
         (bind-key "H-<left>" 'move-beginning-of-line)
         (bind-key "H-f" 'toggle-frame-fullscreen))
  (setq ns-pop-up-frames nil            ; Don't pop up new frames from the workspace
        mac-option-modifier 'meta       ; Option is simply the natural Meta
        mac-command-modifier 'hyper     ; Leave hyper for MacOS Shortcuts
        mac-right-command-modifier 'left
        mac-right-option-modifier 'none ; Keep right option for accented input
        ;; Just in case we ever need these keys
        ;mac-function-modifier 'hyper
        ))

;;; File Handling

(use-package saveplace                  ; Save point position in files
  :init (setq-default save-place t)
  :config (setq save-place-file (concat user-emacs-directory "places")))

;; Keep backup and auto save files out of the way
(setq backup-directory-alist `((".*" . ,(locate-user-emacs-file ".backup")))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Delete files to trash
(setq delete-by-moving-to-trash t)

;; Use native to modern OS way of pasting
(delete-selection-mode 1)

(use-package osx-trash                  ; Trash support for OS X
  :if (eq system-type 'darwin)
  :ensure t
  :init (osx-trash-setup))

(provide 'otann-environment-fixup)
;;; otann-environment-fixup.el ends here
