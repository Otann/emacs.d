;;; otann-helm.el --- separate configuration for messy IDO config
;; Author: Anton Chebotaev <anton.chebotaev@gmail.com>
;; URL: https://gihub.com/otann/.emacs.d
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

(require 'use-package)

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

;; Helm frontend for company
(use-package helm-company
  :ensure t
  :defer t
  :init (with-eval-after-load 'company
          ;; Use Company for completion
          (bind-key [remap completion-at-point] #'helm-company company-mode-map)
          (bind-key "C-:" #'helm-company company-mode-map)
          (bind-key "C-:" #'helm-company company-active-map)))


(provide 'otann-helm)
;;; otann-helm.el ends here
