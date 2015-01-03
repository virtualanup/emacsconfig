;; Emacs Configuration
;; by Virtualanup
;; http://virtualanup.com


(defvar root-dir (file-name-directory load-file-name)
"The root dir of this configuration.")
(defvar temporary-file-directory (concat root-dir "temp"))

(add-to-list 'load-path root-dir)


;;change appearance early during startup
(require 'appearance)

;; Load the required packages
(require 'packages)

;; include some modules
(require 'sessions)
(require 'python)

(setq user-full-name "Anup Pokhrel"
      user-mail-address "virtualanup@gmail.com")

(set-language-environment "UTF-8")

(require 'server)
(unless (server-running-p)
  (server-start))

(require 'general)