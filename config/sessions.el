;; sessions.el
;; Session related settings

(defvar session-dir
  (concat (getenv "HOME") "/.emacs.d/desktop-sessions/")
  "*Directory to save desktop sessions in")

(defvar session-name-history nil
  "Desktop session name history")

;; Session Save
(defun ss (&optional name)
  "Save desktop with a name."
  (interactive)
  (unless name
    (setq name (sessionname "Save session as: ")))
  (make-directory (concat session-dir name) t)
  (desktop-save (concat session-dir name) t)
)

;; Session Open
(defun so (&optional name)
  "Read desktop with a name."
  (interactive)
  (unless name
    (setq name (sessionname "Load session: ")))
  (desktop-read (concat session-dir name)))

(defun sessionname (prompt)
  (completing-read prompt (and (file-exists-p session-dir)
                               (directory-files session-dir))
                   nil nil nil session-name-history))
(provide 'sessions)
