;;; init.el --- Emacs configuration of Sebastian Wiesner -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2012-2015 Anton Chebotaev <anton.chebotaev@gmail.com>
;;
;; Author: Anton Chebotaev <anton.chebotaev@gmail.com>
;; URL: https://gihub.com/otann/.emacs.d
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Emacs config of Anton Chebotaev. Heavily inspired by smilar one from Sebastian Wiesner
;; see https://github.com/lunaryorn/.emacs.d/

;;; Code:

;;; Dumb fixes ;;;

;;; Debugging
(setq message-log-max 10000)

;;; Package management

;; Please don't load outdated byte code
(setq load-prefer-newer t)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;; Requires, builds up `use-package` macro

(eval-when-compile
  (require 'use-package))

(require 'bind-key)
(require 'diminish)

(require 'subr-x)
(require 'rx)
(require 'time-date)

;; Brings some functional practicies to elisp
(use-package dash
  :ensure t
  :demand t)

;;; Customize further customizations
(defconst otann-custom-file (locate-user-emacs-file "custom.el")
  "File used to store settings from Customization UI.")

(use-package cus-edit
  :defer t
  :config (setq custom-file otann-custom-file
		custom-buffer-done-kill nil       ; Kill when existing
		custom-buffer-verbose-help nil    ; Remove redundant help text
		custom-unlispify-tag-names nil    ; Show me the real variable name
		custom-unlispify-menu-entries nil)
  :init (load otann-custom-file 'no-error 'no-message))

;;; All personal configurations is split into modules

;; Load personal modules
(defconst otann-modules "configuration-modules/"
  "Path where all confguration is decomposed.")

;; How emacs interacts with platform
(use-package otann-environment-fixup
  :load-path otann-modules)

;; Themes and bars
(use-package otann-look-and-feel
  :load-path otann-modules)

;; Mode line stands out for it's complexity
(use-package otann-modeline
  :load-path otann-modules)

;; How to move around files and projects
(use-package otann-navigation
  :load-path otann-modules)

;; Development tools
(use-package otann-ide
  :load-path otann-modules)

(provide 'init)
;;; init.el ends here
