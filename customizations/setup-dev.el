;;; Commentary:
;; This describes commoh helpers for all programming features

;;; Code:

;; Code analysys with flycheck
(require 'flycheck)
(setq flycheck-indication-mode 'right-fringe)

;; Prefix all FlyCheck with C-c f
(define-key flycheck-mode-map flycheck-keymap-prefix nil)
(setq flycheck-keymap-prefix (kbd "C-c f"))
(define-key flycheck-mode-map flycheck-keymap-prefix flycheck-command-map)

;; Customize HideShowVis
(hideshowvis-symbols)
(global-set-key (kbd "C-c h") 'hs-toggle-hiding)
(set-face-attribute 'hs-face nil
                    :box nil
                    :background "gray50"
                    :foreground "black")

;; Symbol highlighting
(require 'highlight-symbol)
(global-set-key (kbd "C-c s") 'highlight-symbol-at-point)

;; Shell scripts
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)

;; Set up autocompletion through company-mode
(add-hook 'after-init-hook 'global-company-mode)
(global-set-key (kbd "S-<SPC>") 'company-complete)

;; autocomplete configuration
;(require 'auto-complete-config)
;(setq ac-delay 0.0)
;(setq ac-quick-help-delay 0.0)
;(ac-config-default)

;; Set position of tool windows
(defun add-tool-bottom-window (regex)
  "Adds rule to opening window as bottom tool window"
  (add-to-list 'display-buffer-alist
             `(,regex
               (display-buffer-reuse-window
                display-buffer-in-side-window)
               (reusable-frames . visible)
               (side            . bottom)
               (window-height   . 0.2))))

(defun add-tool-right-window (regex)
  "Adds rule to opening window as bottom tool window"
  (add-to-list 'display-buffer-alist
             `(,regex
               (display-buffer-reuse-window
                display-buffer-in-side-window)
               (reusable-frames . visible)
               (side            . right)
               (window-width    . 0.4))))

;; Add regexes for windows
(add-tool-bottom-window (rx bos "*Flycheck errors*" eos))
(add-tool-bottom-window (rx bos "*Backtrace*" eos))
(add-tool-bottom-window (rx bos "*Messages*" eos))
(add-tool-bottom-window (rx bos "*Compile-Log*" eos))

(add-tool-right-window (rx bos "*magit:"))

(defun otann-quit-bottom-side-windows ()
  "Quit side windows of the current frame."
  (interactive)
  (dolist (window (window-at-side-list))
    (quit-window nil window)))
(global-set-key (kbd "C-c q") #'otann-quit-bottom-side-windows)
