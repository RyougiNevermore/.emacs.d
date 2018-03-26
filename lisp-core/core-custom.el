;; core-custom.el	-*- lexical-binding: t -*-
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
  :group 'extensions
)

;; name
(defcustom my-full-name "ryougi"
  "Set user full name."
  :type 'string
)

;; email
(defcustom my-mail-address "ryougi.nevermore@hotmail.com"
  "Set user email address."
  :type 'string
)

;; melpa
(defcustom my-package-archives 'melpa
  "Set package archives from which to fetch."
  :type '(choice 
                  (const :tag "Melpa" melpa)
                  (const :tag "Emacs-China" emacs-china)
                  (const :tag "Tuna" tuna)
                )
)

;; emoji
(defcustom my-emoji-enabled nil
  "Enable emoji features or not."
  :type 'boolean
)

;; benchmark
(defcustom my-benchmark-enabled nil
  "Enable the init benchmark or not."
  :type 'boolean
)

;; my-org-agenda-dir
(defcustom my-org-agenda-dir "~/Org"
  "Set Org agenda dir."
  :type 'string
)

;; osx key mode
(defcustom osx-key-mode nil
  "Use osx-key-mode."
  :type 'boolean
)

;; swapper Meta(Alt) and Super(Win) key for gnu
(defcustom gnu-swapper-meta-super-key nil
  "Swapper Meta(Alt) and Super(Win) key for gnu."
  :type 'boolean
)

;; swapper Meta(Alt) and Super(Win) key for windows(Cygwin)
(defcustom win-swapper-meta-super-key nil
  "Swapper Meta(Alt) and Super(Win) key for windows(Cygwin)."
  :type 'boolean
)

;; theme
(defcustom my-theme 'default
  "Set color theme."
  :type '(choice
                  (const :tag "Default theme(challenger-deep)" default)
                  (const :tag "Default theme(dracula)" dracula)
                  (const :tag "Dark theme(spacemacs-dark)" spacemacs-dark)
                  (const :tag "Light theme(spacemacs-light)" spacemacs-light)
                  (const :tag "Daylight theme(leuven)" spacemacs-daylight)
                )  
)

;; For Emacs devel
(when (= emacs-minor-version 0)
  (setq package-user-dir (locate-user-emacs-file "elpa-devel"))
  (setq desktop-base-file-name ".emacs-devel.desktop")
  (setq desktop-base-lock-name ".emacs-devel.desktop.lock")
)

;; user-init.el
(setq user-init-file (expand-file-name "user-init.el" user-emacs-directory))
(if (file-exists-p user-init-file) 
  (load user-init-file)
)

(provide 'core-custom)

;;----------------------------------------------------------------------------
;; core-custom.el ends here
;;----------------------------------------------------------------------------
