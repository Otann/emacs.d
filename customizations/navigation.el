;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.

;;
;; IDO config
;;

;; This enables ido in all contexts where it could be useful, not just
;; for selecting buffer and file names
(ido-mode 1)
(ido-everywhere 1)
(require 'ido-ubiquitous)
(ido-ubiquitous-mode 1)
(require 'ido-vertical-mode)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)

;; This allows partial matches, e.g. "tl" will match "Tyrion Lannister"
(setq ido-enable-flex-matching t)
;; Turn this behavior off because it's annoying
(setq ido-use-filename-at-point nil)

;; Don't try to match file across all "work" directories; only match files
;; in the current directory displayed in the minibuffer
(setq ido-auto-merge-work-directories-length -1)

;; Includes buffer names of recently open files, even if they're not
;; open now
(setq ido-use-virtual-buffers t)

;; Enhances M-x to allow easier execution of commands. Provides
;; a filterable list of possible commands in the minibuffer
(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

;; "When several buffers visit identically-named files,
;; Emacs must give the buffers distinct names. The usual method
;; for making buffer names unique adds ‘<2>’, ‘<3>’, etc. to the end
;; of the buffer names (all but one of them).
;; The forward naming method includes part of the file's directory
;; name at the beginning of the buffer name
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Uniquify.html
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Turn on recent file mode so that you can more easily switch to
;; recently edited files when you first start emacs
(setq recentf-save-file (concat user-emacs-directory ".recentf"))
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 40)
(global-set-key (kbd "H-r") 'ido-recentf-open)

(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))


;; Shows a list of buffers
(global-set-key (kbd "H-e") 'ibuffer)
;; Shows a tree view
(global-set-key (kbd "H-p") 'neotree-toggle)
;; Switch to Mac Fullscreen space
(global-set-key (kbd "H-F") 'toggle-frame-fullscreen)

;; projectile everywhere!
(projectile-global-mode)

;; assign number for each window
(window-numbering-mode)
(global-set-key (kbd "H-1") 'select-window-1)
(global-set-key (kbd "H-2") 'select-window-2)
(global-set-key (kbd "H-3") 'select-window-3)
(global-set-key (kbd "H-4") 'select-window-4)
(global-set-key (kbd "H-5") 'select-window-5)
(global-set-key (kbd "H-6") 'select-window-6)
(global-set-key (kbd "H-7") 'select-window-7)

;; use Shift+arrow_keys to move cursor around split panes
;(windmove-default-keybindings)

;; Bind movements around panels
;(global-set-key (kbd "H-<right>") 'windmove-right)
;(global-set-key (kbd "H-<left>")  'windmove-left)
;(global-set-key (kbd "H-<up>")    'windmove-up)
;(global-set-key (kbd "H-<down>")  'windmove-down)

;; Mac specific movements
(global-set-key (kbd "H-<right>") 'move-end-of-line)
(global-set-key (kbd "H-<left>")  'move-beginning-of-line)

;; Even closing things
(global-set-key (kbd "H-w") (lambda () (interactive) (kill-buffer)))
(global-set-key (kbd "H-q") 'save-buffers-kill-terminal)

;; Provide helm when there are too many commands
(require 'guide-key)
(setq guide-key/idle-delay 1)
(setq guide-key/guide-key-sequence t)
(guide-key-mode 1)  ; Enable guide-key-mode
