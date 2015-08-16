(add-to-list 'custom-theme-load-path (concat root-dir "themes"))

(load-theme 'solarized-dark t)

;;nice line numbering
(global-linum-mode t)
(setq linum-format " %d ")
(column-number-mode t)
(size-indication-mode t)

;; remove the scroll bar
(scroll-bar-mode -1)

;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)

;;remove the menu bar
(menu-bar-mode -1)

;; remove the toolbar
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)


(custom-set-faces
 '(flymake-errline ((((class color)) (:background nil))))
 '(flymake-warnline ((((class color)) (:background nil)))))

(provide 'appearance)