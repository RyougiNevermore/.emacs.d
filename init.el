
;; -*- lexical-binding: t -*-

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq debug-on-error t)

;; lisp
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; spell check support
(defconst *spell-check-support-enabled* t)
;; system is darwin 
(defconst *is-a-mac* (eq system-type 'darwin))



;;----------------------------------------------------------------------------
;; Bootstrap config
;;----------------------------------------------------------------------------


;;----------------------------------------------------------------------------
;; Load configs for specific features and modes
;;----------------------------------------------------------------------------
(require 'basic)
(require 'system-kbd)
(require 'packages-elpa)
(require 'theme)
(require 'recentfile)
(require 'init-company)


;;----------------------------------------------------------------------------
;; Load custom file
;;----------------------------------------------------------------------------
(setq custom-file (expand-file-name "lisp/custom.el" user-emacs-directory))
(load-file custom-file)
