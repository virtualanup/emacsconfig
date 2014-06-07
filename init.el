
;; add this directory to the load path
(add-to-list 'load-path user-emacs-directory)

;;change appearance early during startup
(require 'appearance)

(setq inhibit-startup-message   t)   ; Don't want any startup message
(setq make-backup-files         nil) ; Don't want any backup files
(setq auto-save-list-file-name  nil) ; Don't want any .saves files
(setq auto-save-default         nil) ; Don't want any auto saving 

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; enable y/n answers instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; show file or buffer name
(setq frame-title-format
      '("" invocation-name " : virtualanup - " (:eval (if (buffer-file-name)
                                            (abbreviate-file-name (buffer-file-name))
                                          "%b"))))
;; Start eshell or switch to it if it's active.
(global-set-key (kbd "C-x m") 'eshell)

(defun virtualanup-fullscreen ()
  "Make Emacs window fullscreen.

This follows freedesktop standards, should work in X servers."
  (interactive)
  (if (eq window-system 'x)
      (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                             '(2 "_NET_WM_STATE_FULLSCREEN" 0))
    (error "Only X server is supported")))

(unless (fboundp 'toggle-frame-fullscreen)
  (global-set-key (kbd "<f11>") 'virtualanup-fullscreen))



(setq-default indent-tabs-mode nil)   ;; don't use tabs to indent
(setq-default tab-width 8)            ;; but maintain correct appearance

(ido-mode t) ;load ido mode
(iswitchb-mode t) ; load iswitchb mode

;; <enter> key automatically indents in programming mode
(defun my-coding-config ()
  (local-set-key (kbd "RET") (key-binding (kbd "C-j")))
  (local-set-key (kbd "<S-return>") 'newline)
  )

(mapc
 (lambda (language-mode-hook)
   (add-hook language-mode-hook 'my-coding-config))
 '(prog-mode-hook
   ))

;; set the default tabs in c, c++ etc to 4 spaces
 (setq
          c-basic-offset 4)

(defun replace-last-sexp ()
    (interactive)
    (let ((value (eval (preceding-sexp))))
      (kill-sexp -1)
      (insert (format "%S" value))))

(windmove-default-keybindings)
(setq windmove-wrap-around t)
(global-set-key [(control h)] 'delete-backward-char)
