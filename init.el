
;; -*- lexical-binding: t -*-

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

(when (version< emacs-version "24.4")
  (error "This requires Emacs 24.4 and above!"))

;; Optimize loading performance
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(setq gc-cons-threshold 30000000)
(add-hook 'emacs-startup-hook
          (lambda ()
            "Restore defalut values after init"
            (setq file-name-handler-alist default-file-name-handler-alist)
            (setq gc-cons-threshold 800000)))


;; Load path
(add-to-list 'load-path (expand-file-name "lisp-core" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp-preference" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp-module" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))


;;----------------------------------------------------------------------------
;; Bootstrap config
;;----------------------------------------------------------------------------
;; Constants
(require 'core-const)

;; Customization
(require 'core-custom)

;; package
;; Without this comment Emacs25 adds (package-initialize) here
(package-initialize)
(require 'core-package)


;;----------------------------------------------------------------------------
;; Preferences
;;----------------------------------------------------------------------------
(require 'preference-basic)
(require 'preference-ui)


;;----------------------------------------------------------------------------
;; Programming
;;----------------------------------------------------------------------------




;;----------------------------------------------------------------------------
;; init.el ends here
;;----------------------------------------------------------------------------
