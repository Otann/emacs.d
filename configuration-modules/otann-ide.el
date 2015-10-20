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
(setq-default indent-tabs-mode nil)

;; Use delight to set prefereed names for major modes
(use-package delight
  :ensure t
  :demand t)

;; Paired delimiters - Parenthesis editing and balancing
(use-package smartparens
;  :disabled t
  :ensure t
  :init (progn (smartparens-global-mode)
               (show-smartparens-global-mode)
               ;; make mode autoload with certain modes
               (dolist (hook '(inferior-emacs-lisp-mode-hook
                               emacs-lisp-mode-hook))
                       (add-hook hook #'smartparens-strict-mode)))
  :config (progn
            ;; do not autoinsert ' pair if the point is preceeded by word.  This
            ;; will handle the situation when ' is used as a contraction symbol in
            ;; natural language.  Nil for second argument means to keep the
            ;; original definition of closing pair.
            (sp-pair "'" "'"
                     :unless '(sp-point-after-word-p sp-point-before-word-p))

            (sp-pair "\"" "\""
                     :unless '(sp-point-after-word-p sp-point-before-word-p)
                     :actions '(wrap insert autoskip))

            (setq sp-autoskip-closing-pair 'always
                  sp-autoskip-opening-pair nil
                  ;; Don't kill entire symbol on C-k
                  sp-hybrid-kill-entire-symbol nil
                  sp-autoescape-string-quote nil)

            (let ((map smartparens-mode-map))
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
              ;;
              (define-key map (kbd "C-(") #'sp-backward-slurp-sexp)
              (define-key map (kbd "C-M-<left>") #'sp-backward-slurp-sexp)
              (define-key map (kbd "C-{") #'sp-backward-barf-sexp)
              (define-key map (kbd "C-M-<right>") #'sp-backward-barf-sexp)
              ;; Miscellaneous commands
              (define-key map (kbd "C-c k s") #'sp-split-sexp)
              (define-key map (kbd "C-c k j") #'sp-join-sexp)
              (define-key map (kbd "C-M-t") #'sp-transpose-sexp)))
  :diminish (smartparens-mode . "ⓟ"))

;; Good old paredit
(use-package paredit
  :disabled t
  :ensure t
  :config (progn
            (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
            (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
            (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
            (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
            (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
            (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
            (add-hook 'scheme-mode-hook           #'enable-paredit-mode))
  :diminish (paredit-mode . "ⓟ"))

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

            ;; disable jshint since we prefer eslint checking
            (setq-default flycheck-disabled-checkers
                          (append flycheck-disabled-checkers
                                  '(javascript-jshint)))

            ;; use eslint with web-mode for jsx files
            (flycheck-add-mode 'javascript-eslint 'js2-mode)

            ;; Use italic face for checker name
            (set-face-attribute 'flycheck-error-list-checker-name nil
                                :inherit 'italic))
  :diminish flycheck-mode)

;;; WakaTime track time spent in projects
;; custom key supposed to be in custom.el
;; @see https://wakatime.com/help/plugins/emacs
(use-package wakatime-mode
  :ensure t
  :config (global-wakatime-mode)
  :diminish wakatime-mode)

;; Subword/superword editing
(use-package subword
  :defer t
  :diminish (subword-mode . "ⓢ"))

;; Install code-folding
(use-package hideshowvis
  :ensure t
  :config (progn
            (hideshowvis-symbols)
            ;; For some reason :diminish does not work with it
            (delight 'hs-minor-mode "ⓧ" 'hideshow) ; ¶⧒
            (bind-key "C-c h" 'hs-toggle-hiding)
            (set-face-attribute 'hs-face nil
                                :box nil
                                :background (face-foreground 'default)
                                :foreground (face-background 'default))
            (add-hook 'emacs-lisp-mode-hook 'hideshowvis-minor-mode)
            (add-hook 'js2-mode 'hideshowvis-minor-mode)
            (add-hook 'clojure-mode 'hideshowvis-minor-mode)))

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

(use-package diff-hl
  :ensure t
  :defer t
  :init (progn
          ;; Highlight changes to the current file in the fringe
          (global-diff-hl-mode)
          ;; Highlight changed files in the fringe of Dired
          (add-hook 'dired-mode-hook 'diff-hl-dired-mode)

          ;; Highlight changes on the fly
          (diff-hl-flydiff-mode)

          ;; Prefer fringe
          (diff-hl-margin-mode)))

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

;;; Languages Modes
;; html editing
(use-package web-mode
  :ensure web-mode
  :defer t
  :mode "\\.html\\'"
  :config (setq web-mode-markup-indent-offset 2
                web-mode-css-indent-offset 2
                web-mode-code-indent-offset 2))

;; JS/ES editing
(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :config (progn (delight 'js2-mode "js2" :major)

                 (defun my-js2-mode-hook ()
                   (interactive)
                   #'js2-highlight-unused-variables-mode
                   ; Scan the file for nested code blocks
                   (imenu-add-menubar-index)
                   ; Enable code folding
                   (hideshowvis-enable)
                   ; more defaults
                   (setq js2-global-externs '("angular")
                         js2-strict-missing-semi-warning nil
                         js2-missing-semi-one-line-override nil
                         js2-indent-switch-body t)

                   (setq-default js2-basic-offset 2
                               js-indent-level 2))

                 (add-hook 'js2-mode-hook 'my-js2-mode-hook)))

(use-package yasnippet
  :diminish yas-minor-mode)

(use-package javascript-mode
  :disabled t)

(use-package js2-refactor
  :ensure t
  :config (add-hook 'js2-mode-hook #'js2-refactor-mode)
  :diminish js2-refactor-mode)

(use-package json-mode
  :ensure t
  :defer t)

(use-package json-reformat
  :ensure t
  :defer t
  :bind (("C-c x j" . json-reformat-region)))

;; Markdown
(use-package markdown-mode
  :ensure t
  ;; Just no, dear Markdown Mode.  Don't force that bastard Github dialect upon
  ;; me!
  :mode ("\\.md\\'" . markdown-mode)
  :config (progn
            ;; Unbind MacOS movement keys
            (unbind-key "M-<left>" markdown-mode-map)
            (unbind-key "M-<right>" markdown-mode-map)
            (unbind-key "M-S-<right>" markdown-mode-map)
            (unbind-key "M-S-<left>" markdown-mode-map)

            ;; No filling in GFM, because line breaks are significant.
            (add-hook 'gfm-mode-hook #'turn-off-auto-fill)
            ;; Use visual lines instead
            (add-hook 'gfm-mode-hook #'visual-line-mode)
            (add-hook 'gfm-mode-hook #'lunaryorn-whitespace-style-no-long-lines)

            (bind-key "C-c C-s C" #'markdown-insert-gfm-code-block markdown-mode-map)
            (bind-key "C-c C-s P" #'markdown-insert-gfm-code-block markdown-mode-map)

            ;; Fight my habit of constantly pressing M-q.  We should not fill in GFM
            ;; Mode.
            (bind-key "M-q" #'ignore gfm-mode-map)))

;; Shell scripts
(use-package sh-script
  :mode ("\\.zsh\\'" . sh-mode)
  :config
  ;; Use two spaces in shell scripts.
  (setq
   ;; The basic indentation
   sh-indentation 2
   ; The offset for nested indentation
   sh-basic-offset 2))

(use-package yaml-mode
  :ensure t
  :config (delight 'yaml-mode "yaml " :major))

;;; Clojure
(use-package clojure-mode
  :ensure t
  :mode "\\.\\(clj\\|cljs\\|edn\\|lein-env\\|boot\\)\\\\'"
  :config (progn (delight 'clojure-mode "clj " :major)

                 (use-package clojure-mode-extra-font-locking
                   :ensure t)

                 (defun my-clojure-hook ()
                   (interactive)
                   (setq inferior-lisp-program "lein repl")
                   (font-lock-add-keywords
                    nil
                    '(("(\\(facts?\\)" (1 font-lock-keyword-face))
                      ("(\\(background?\\)" (1 font-lock-keyword-face))))
                   (define-clojure-indent (fact 1))
                   ;; Enable code folding
                   (hideshowvis-enable)
                   ;; Deal with parenthesis
                   #'smartparens-strict-mode)

                 (add-hook 'clojure-mode-hook #'my-clojure-hook)))

(use-package cider
  :ensure t
  :diminish (cider-mode . "ⓓ")
  :config (progn
            ;; provides minibuffer documentation for the
            ;; code you're typing into the repl
            (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

            (setq
             ;; go right to the REPL buffer when it's finished connecting
             cider-repl-pop-to-buffer-on-connect t
             ;;  When there's a cider error, show its buffer and switch to it
             cider-show-error-buffer t
             cider-auto-select-error-buffer t

             ;; Where to store the cider history.
             cider-repl-history-file "~/.emacs.d/cider-history"

             ;; Wrap when navigating history.
             cider-repl-wrap-history t)

            (add-hook 'cider-repl-mode-hook 'smartparens-strict-mode)))

;; I love this one, but let's keep it hidden
(use-package eldoc
  :diminish eldoc-mode)

(use-package terraform-mode
  :ensure t
  :mode ("\\.tf\\'" . terraform-mode))

;(use-package foreman-mode
;  :ensure t
;  :mode ("\\Procfile\\'" . foreman-mode))

(provide 'otann-ide)
;;; otann-ide.el ends here
