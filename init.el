;; Emacs Configuration
;; by Virtualanup
;; http://virtualanup.com


(defvar root-dir user-emacs-directory
"The root dir of this configuration.")
(defvar temporary-file-directory (concat root-dir "temp"))

(add-to-list 'load-path root-dir)


;;change appearance early during startup
(require 'appearance)

;; Load the required packages
(require 'packages)

;; include some modules
(require 'sessions)

(setq user-full-name "Anup Pokhrel"
      user-mail-address "virtualanup@gmail.com")

(set-language-environment "UTF-8")

(require 'server)
(unless (server-running-p)
  (server-start))

(require 'general)