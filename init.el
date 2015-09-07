;; Emacs Configuration
;; by Virtualanup
;; http://virtualanup.com


(defvar root-dir (file-name-directory load-file-name)
"The root dir of this configuration.")
(defvar temporary-file-directory (concat root-dir "temp"))

(defvar config-directory (concat root-dir "config"))

(add-to-list 'load-path config-directory)

;; Load the required packages
(require 'packages)

;;change appearance early during startup
(require 'appearance)

;; include some modules
(require 'sessions)

(setq user-full-name "Anup Pokhrel"
      user-mail-address "virtualanup@gmail.com")

(set-language-environment "UTF-8")

(require 'server)
(unless (server-running-p)
  (server-start))

(require 'general)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
