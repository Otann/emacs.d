;;; otann-ido.el --- separate configuration for messy IDO config
;; Author: Anton Chebotaev <anton.chebotaev@gmail.com>
;; URL: https://gihub.com/otann/.emacs.d
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

(require 'use-package)

(use-package ido
  :init (progn
          (ido-mode 1)
          (ido-everywhere 1)

          (use-package ido-ubiquitous
            :ensure t
            :init (ido-ubiquitous-mode 1))

          (use-package ido-vertical
            :init (progn
                    (ido-vertical-mode 1)
                    (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)))

          (use-package smex
            :init (smex-initialize)
            :bind ("M-x" . smex)))


  :config (progn
            (setq ido-everywhere t)
            ;; This allows partial matches, e.g. "tl" will match "Tyrion Lannister"
            (setq ido-enable-flex-matching t)
            ;; Turn this behavior off because it's annoying
            (setq ido-use-filename-at-point nil)
            (setq ido-max-prospects 10)
            (setq ido-case-fold t)
            (setq ido-enable-prefix nil)
            (setq ido-create-new-buffer 'always)
            ;; (setq ido-use-faces nil)
            (setq ido-file-extensions-order '(".rb" ".el" ".coffee" ".js"))
            (add-to-list 'ido-ignore-files "\\.DS_Store")
            ;; Don't try to match file across all "work" directories; only match files
            ;; in the current directory displayed in the minibuffer
            (setq ido-auto-merge-work-directories-length -1)
            ;; Includes buffer names of recently open files,
            ;; even if they're not open now
            (setq ido-use-virtual-buffers t)))

(provide 'otann-ido)
;;; otann-ido.el ends here
