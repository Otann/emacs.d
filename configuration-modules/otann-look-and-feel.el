;;; otann-look-and-feel.el --- describes how emacs looks and feels
;; Author: Anton Chebotaev <anton.chebotaev@gmail.com>
;; URL: https://gihub.com/otann/.emacs.d
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

(require 'dash)
(require 'use-package)
(require 'bind-key)

;;; Initial window size
(setq initial-frame-alist '((top . 0)
			    (left . 0)
			    (width . 180)
			    (height . 53)))

;;; Fonts
;;
;; - 'Source Code Pro' as default
;; - 'Hack' is a good one also
;;
;; Find more fonts for that are good for everyday
;; development work here: https://github.com/powerline/fonts

(set-face-attribute 'default nil
                    :family "Source Code Pro for Powerline"
                    :height 120
                    :weight 'medium)
(set-face-attribute 'variable-pitch nil
                    :family "Source Code Pro for Powerline"
                    :height 160
                    :weight 'regular)

;; Font setup
(defun otann-configure-fonts (frame)
  "Set up fonts for FRAME.
Set the default font, and configure various overrides for
symbols, emojis, greek letters, as well as fall backs for."
  ;; Additional fonts for special characters and fallbacks
  ;; Test range: 🐷 ❤ ⊄ ∫ 𝛼 α 🜚 Ⓚ

  (dolist (script '(symbol mathematical))
    (set-fontset-font t script (font-spec :family "XITS Math")
                      frame 'prepend))

  ;; Define a font set stack for symbols, greek and math characters
  (dolist (script '(symbol greek mathematical))
    (set-fontset-font t script (font-spec :family "Arial Unicode MS")
                      frame 'prepend)
    (set-fontset-font t script (font-spec :family "Menlo")
                      frame 'prepend)
    (set-fontset-font t script (font-spec :family "DejaVu Sans Mono")
                      frame 'prepend)
    (set-fontset-font t script (font-spec :family "Monoid" :weight 'light)
                      frame 'prepend))

  (when (eq system-type 'darwin)
    ;; Colored Emoji on OS X, prefer over everything else!
    (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji")
                      frame 'prepend))

  ;; Fallbacks for math and generic symbols
  (set-fontset-font t nil (font-spec :family "Apple Symbols")
                    frame 'append))

(-when-let (frame (selected-frame))
  (otann-configure-fonts frame))
(add-hook 'after-make-frame-functions #'otann-configure-fonts)

;;; Bells and whistles

;; Get rid of tool bar, menu bar and scroll bars.  On OS X we preserve the menu
;; bar, since the top menu bar is always visible anyway, and we'd just empty it
;; which is rather pointless.
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (and (not (eq system-type 'darwin))
	   (fboundp 'menu-bar-mode))
  (menu-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; No blinking and beeping, no startup screen, no scratch message and short
;; Yes/No questions.
(blink-cursor-mode -1)
(setq ring-bell-function #'ignore
      inhibit-startup-screen t
      initial-scratch-message "Hello there!\n")
(fset 'yes-or-no-p #'y-or-n-p)
;; Opt out from the startup message in the echo area by simply disabling this
;; ridiculously bizarre thing entirely.
(fset 'display-startup-echo-area-message #'ignore)
;; full path in title bar
(setq-default frame-title-format "%b (%f)")

;;; Theme

(use-package solarized
  :ensure solarized-theme
  :config
  ;; Disable variable pitch fonts in Solarized theme
  (progn (setq solarized-use-variable-pitch nil
               ;; Prefer italics over bold
               ;solarized-use-less-bold t
               ;solarized-use-more-italic t
               solarized-high-contrast-mode-line t
               solarized-distinct-fringe-background nil
               ;; I find different font sizes irritating.
               solarized-height-minus-1 1.0
               solarized-height-plus-1  1.0
               solarized-height-plus-2  1.0
               solarized-height-plus-3  1.0
               solarized-height-plus-4  1.0)

         (load-theme 'solarized-dark 'no-confirm)))

;;; Navigation

; Show help popups for prefix keys
(use-package which-key
  :ensure t
  :init (which-key-mode)
  :config (setq which-key-idle-delay 0.4
                which-key-popup-type 'side-window
                which-key-side-window-location 'right
                which-key-side-window-max-width 0.33
                which-key-side-window-max-height 0.25
                which-key-key-replacement-alist
                '(("<\\([[:alnum:]-]+\\)>" . "\\1")
                  ("up"                    . "↑")
                  ("right"                 . "→")
                  ("down"                  . "↓")
                  ("left"                  . "←")
                  ("DEL"                   . "⌫")
                  ("deletechar"            . "⌦")
                  ("RET"                   . "⏎"))
                which-key-description-replacement-alist
                '(("Prefix Command" . "prefix")
                  ("\\`\\?\\?\\'"   . "λ")
                  ;; Remove my personal prefix from all bindings, since it's
                  ;; only there to avoid name clashes, but doesn't add any value
                  ;; at all
                  ("otann-"     . "")))
  :diminish (which-key-mode . "Ⓚ"))

;; "When several buffers visit identically-named files,
;; Emacs must give the buffers distinct names. The usual method
;; for making buffer names unique adds ‘<2>’, ‘<3>’, etc. to the end
;; of the buffer names (all but one of them).
;; The forward naming method includes part of the file's directory
;; name at the beginning of the buffer name
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Uniquify.html
(use-package uniquify
  :config (setq uniquify-buffer-name-style 'forward))

(provide 'otann-look-and-feel)
;;; otann-look-and-feel.el ends here
