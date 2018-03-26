
;;; init.el --- user init configuration.	-*- lexical-binding: t no-byte-compile: t; -*-
;;
;; Filename: init.el
;; Description:
;; Author: Ryougi Nevermore <ryougi.nevermore@hotmail.com>
;; Version: 1.1.0
;; Maintainer:
;; Created: 2018-03-25
;; URL: https://github.com/RyougiNevermore/.emacs.d
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;             Ryougi's Emacs configuration
;;
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

;;----------------------------------------------------------------------------
;; Version required
;;----------------------------------------------------------------------------

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(when (version< emacs-version "25.0")
  (error "This requires Emacs 25.0 and above!"))

;;----------------------------------------------------------------------------
;; Optimize loading performance
;;----------------------------------------------------------------------------

(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(setq gc-cons-threshold 30000000)
(add-hook 'emacs-startup-hook
          (lambda ()
            "Restore defalut values after init"
            (setq file-name-handler-alist default-file-name-handler-alist)
            (setq gc-cons-threshold 800000)))

;;----------------------------------------------------------------------------
;; Bootstrap config
;;----------------------------------------------------------------------------

;; Load path
(add-to-list 'load-path (expand-file-name "lisp-core" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp-mode" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp-lang" user-emacs-directory))

;;----------------------------------------------------------------------------
;; Core
;;---------------------------------------------------------------------------

;; Constants
(require 'core-const)

;; Customization
(require 'core-custom)

;; Package
(require 'core-package)

;; Preferences
(require 'core-preference)

;; Edit
(require 'core-edit)

;; Modeline
(require 'core-modeline)

;; Tree
(require 'core-tree)

;; High Light
(require 'core-highlight)

;;  Recent file
(require 'core-recentf)

;; Functions
;;(require 'core-func)

;;----------------------------------------------------------------------------
;; Mode
;;----------------------------------------------------------------------------

;; Projectile
;;(require 'mode-projectile)

;; Ivy
;;(require 'mode-ivy)

;; Evil
;;(require 'mode-evil)

;; Which key
;;(require 'mode-which-key)

;; Window
;;(require 'mode-window)

;; Company

;; Yasnippet

;; Flycheck

;; Dired

;; Kill-ring

;; Search

;; eshell

;; Shell

;; VCS

;; Dictionay


;;----------------------------------------------------------------------------
;; Lang
;;----------------------------------------------------------------------------

;; GO

;; WEB

;; NODEJS



;;----------------------------------------------------------------------------
;; init.el ends here
;;----------------------------------------------------------------------------
