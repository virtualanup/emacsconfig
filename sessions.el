;; sessions.el
;; Session related settings

(defvar session-dir
  (concat (getenv "HOME") "/.emacs.d/desktop-sessions/")
  "*Directory to save desktop sessions in")

(defvar session-name-history nil
  "Desktop session name history")

(defun session-save (&optional name)
  "Save desktop with a name."
  (interactive)
  (unless name
    (setq name (session-name "Save session as: ")))
  (make-directory (concat session-dir name) t)
  (desktop-save (concat session-dir name) t))

(defun session-load (&optional name)
  "Read desktop with a name."
  (interactive)
  (unless name
    (setq name (session-name "Load session: ")))
  (desktop-read (concat session-dir name)))

(defun session-name (prompt)
  (completing-read prompt (and (file-exists-p session-dir)
                               (directory-files session-dir))
                   nil nil nil session-name-history))
(provide 'sessions)