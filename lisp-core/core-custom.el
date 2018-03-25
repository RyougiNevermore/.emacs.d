;; core-custom.el --- Initialize custom configurations.	-*- lexical-binding: t -*-
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;             Custom configurations.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; License:
;;             GNU GENERAL PUBLIC LICENSE
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;;; Code:

;; conf
(defgroup my nil
  "Personal Emacs configurations."
  :group 'extensions)


;; name
(defcustom my-full-name "ryougi"
  "Set user full name."
  :type 'string)

;; email
(defcustom my-mail-address "ryougi.nevermore@hotmail.com"
  "Set user email address."
  :type 'string)

;; melpa
(defcustom my-package-archives 'melpa
  "Set package archives from which to fetch."
  :type '(choice
          (const :tag "Melpa" melpa)
          (const :tag "Emacs-China" emacs-china)
          (const :tag "Tuna" tuna)))

;; emoji
(defcustom my-emoji-enabled nil
  "Enable emoji features or not."
  :type 'boolean)

;; benchmark
(defcustom my-benchmark-enabled nil
  "Enable the init benchmark or not."
  :type 'boolean)

;; my-org-agenda-dir
(defcustom my-org-agenda-dir "~/Org"
  "Set Org agenda dir."
  :type 'string)

;; For Emacs devel
(when (= emacs-minor-version 0)
  (setq package-user-dir (locate-user-emacs-file "elpa-devel"))
  (setq desktop-base-file-name ".emacs-devel.desktop")
  (setq desktop-base-lock-name ".emacs-devel.desktop.lock"))

;; custom.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(if (file-exists-p custom-file)
    (load custom-file))

(provide 'core-custom)


;;----------------------------------------------------------------------------
;; core-custom.el ends here
;;----------------------------------------------------------------------------
