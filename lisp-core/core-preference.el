;; core-preference.el --- Desfine constants.	-*- lexical-binding: t -*-
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;             Preferences..
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

(eval-when-compile
  (require 'core-const)
  (require 'core-custom)
  (require 'core-package))

;; personal information
(setq user-full-name my-full-name)
(setq user-mail-address my-mail-address)

;; disable backup files  
(setq make-backup-files nil)
;; disable auto save
(setq auto-save-default nil)
;; deleting files go to OS's trash folder
(setq delete-by-moving-to-trash t)
;; disable ring bell
(setq ring-bell-function 'ignore)
;; enable auto revert
(global-auto-revert-mode t)
;; enable abbrev
(abbrev-mode t)

;; Menu/Tool/Scroll bars
(unless (or sys/mac-x-p sys/linux-x-p) (menu-bar-mode -1))
(and (bound-and-true-p tool-bar-mode) (tool-bar-mode -1))
(and (bound-and-true-p scroll-bar-mode) (scroll-bar-mode -1))
(and (bound-and-true-p horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1))

;; show line number
(global-linum-mode t)
;; set cursor type
(setq-default cursor-type 'bar)
;; enable hight line
(global-hl-line-mode t)
;; max screen
(setq  initial-frame-alist (quote ((fullscreen . maximized))))


;; Line and Column
(setq-default fill-column 80)
(setq column-number-mode t)
(setq line-number-mode t)



(provide 'core-preference)
;;----------------------------------------------------------------------------
;; core-preference.el ends here
;;----------------------------------------------------------------------------