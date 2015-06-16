
;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)


;; No need for ~ files when editing
(setq create-lockfiles nil)

;; Go straight to scratch buffer on startup
(setq inhibit-startup-message t)

;; Toying around
;(setq frame-title-format "GNU Emacs: %b")
; mode-line-format
; header-line-format
; 
; face mode-line
; face mode-line-inactive

;; Magit & Ediff
(setq magit-last-seen-setup-instructions "1.4.0")
(setq ediff-split-window-function 'split-window-horizontally)

;; From Steve Yegge
;; someday might want to rotate windows if more than 2 of them
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
  (interactive "sNew name: ")
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

;; auto-reload changed files
(global-auto-revert-mode t)

;; enable global diff
(global-diff-hl-mode)
(diff-hl-margin-mode)

;; WakaTime
;; @see https://wakatime.com/help/plugins/emacs
(global-wakatime-mode)
