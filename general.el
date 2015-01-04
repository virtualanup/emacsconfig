;; General Settings


(global-hl-line-mode) ; highlight current line


;; show file or buffer name
(setq frame-title-format
'("virtualanup - " (:eval (if (buffer-file-name)
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



;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
`((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
`((".*" ,temporary-file-directory t)))
;; autosave the undo-tree history
(setq undo-tree-history-directory-alist
`((".*" . ,temporary-file-directory)))
(setq undo-tree-auto-save-history t)


;; (ido-mode t) ;load ido mode
(setq ido-enable-prefix nil
ido-enable-flex-matching t ;; enable fuzzy matching
ido-auto-merge-work-directories-length nil
ido-create-new-buffer 'always
ido-use-filename-at-point 'guess
ido-use-virtual-buffers t
ido-handle-duplicate-virtual-buffers 2
ido-max-prospects 10
)

;; load iswitchb mode
;; (iswitchb-mode t) 

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


;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)


;; meaningful names for buffers with the same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers



;; flyspell-mode does spell-checking on the fly as you type
(require 'flyspell)
(setq ispell-program-name "aspell" ; use aspell instead of ispell
ispell-extra-args '("--sug-mode=ultra"))

(defun enable-flyspell ()
"Enable command `flyspell-mode' "
(when (executable-find ispell-program-name)
(flyspell-mode +1)))

(enable-flyspell)

;; Use shell-like backspace C-h, rebind help to F1
(define-key key-translation-map [?\C-h] [?\C-?])
(global-set-key (kbd "<f1>") 'help-command)
;;use alt-h for deleting previous word
(global-set-key (kbd "M-h") 'backward-kill-word)

(setq-default indent-tabs-mode nil) ;; don't use tabs to indent
(setq-default tab-width 4) ;; but maintain correct appearance

(setq inhibit-startup-message t) ; Don't want any startup message
(setq make-backup-files nil) ; Don't want any backup files
(setq auto-save-list-file-name nil) ; Don't want any .saves files
(setq auto-save-default nil) ; Don't want any auto saving

;; Transpose stuff with M-t
(global-unset-key (kbd "M-t")) ;; which used to be transpose-words
(global-set-key (kbd "M-t l") 'transpose-lines)
(global-set-key (kbd "M-t w") 'transpose-words)
(global-set-key (kbd "M-t s") 'transpose-sexps)
(global-set-key (kbd "M-t p") 'transpose-params)
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c C-k") 'eval-buffer)

;; enable y/n answers instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; nice scrolling
(setq scroll-margin 0
scroll-conservatively 100000
scroll-preserve-screen-position 1)


;; Expand Region
(global-set-key (kbd "C-=") 'er/expand-region)


;; Ace Jump
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;; Ace window
(global-unset-key (kbd "M-o"))
(global-set-key (kbd "M-o") 'ace-window)

;; Anzu settings
(global-anzu-mode +1)
(global-set-key (kbd "M-%") 'anzu-query-replace)
(global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)


;; Diff-hl mode
;; (setq diff-hl-fringe-bmp-function 'diff-hl-fringe-bmp-from-type)
;; (global-diff-hl-mode 1)

;; Easy Kill
(global-set-key [remap kill-ring-save] 'easy-kill)
(global-set-key [remap mark-sexp] 'easy-mark)

;; Guru mode
(guru-global-mode +1)
(add-hook 'prog-mode-hook 'guru-mode)

;; Projectile
(projectile-global-mode)


;; Multiple Cursors
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)


;; Helm
(global-set-key (kbd "M-x") 'helm-M-x)
(helm-mode 1)


(defun ido-imenu ()
"Update the imenu index and then use ido to select a symbol to navigate to.
Symbols matching the text at point are put first in the completion list."
(interactive)
(imenu--make-index-alist)
(let ((name-and-pos '())
(symbol-names '()))
(flet ((addsymbols (symbol-list)
(when (listp symbol-list)
(dolist (symbol symbol-list)
(let ((name nil) (position nil))
(cond
((and (listp symbol) (imenu--subalist-p symbol))
(addsymbols symbol))
 
((listp symbol)
(setq name (car symbol))
(setq position (cdr symbol)))
 
((stringp symbol)
(setq name symbol)
(setq position (get-text-property 1 'org-imenu-marker symbol))))
 
(unless (or (null position) (null name))
(add-to-list 'symbol-names name)
(add-to-list 'name-and-pos (cons name position))))))))
(addsymbols imenu--index-alist))
;; If there are matching symbols at point, put them at the beginning of `symbol-names'.
(let ((symbol-at-point (thing-at-point 'symbol)))
(when symbol-at-point
(let* ((regexp (concat (regexp-quote symbol-at-point) "$"))
(matching-symbols (delq nil (mapcar (lambda (symbol)
(if (string-match regexp symbol) symbol))
symbol-names))))
(when matching-symbols
(sort matching-symbols (lambda (a b) (> (length a) (length b))))
(mapc (lambda (symbol) (setq symbol-names (cons symbol (delete symbol symbol-names))))
matching-symbols)))))
(let* ((selected-symbol (ido-completing-read "Symbol? " symbol-names))
(position (cdr (assoc selected-symbol name-and-pos))))
(goto-char position)))) 

;; Push mark when using ido-imenu
(defvar push-mark-before-goto-char nil)
 
(defadvice goto-char (before push-mark-first activate)
(when push-mark-before-goto-char
(push-mark)))
 
(defun ido-imenu-push-mark ()
(interactive)
(let ((push-mark-before-goto-char t))
(ido-imenu))) 
(provide 'general)