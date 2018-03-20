;; preference-basic.el --- Desfine constants.	-*- lexical-binding: t -*-
;;
;; Author: Ryougi Nevermore <ryougi.nevermore@hotmail.com>
;; Version: 0.0.1
;; URL: https://github.com/RyougiNevermore/.emacs.d
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;             Basic configurations.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:


(eval-when-compile
  (require 'core-const)
  (require 'core-custom)
  (require 'core-package))

;; Personal information
(setq user-full-name my-full-name)
(setq user-mail-address my-mail-address)

;; disable backup files  
(setq make-backup-files nil)
;; disable auto save
(setq auto-save-default nil)
;; enable delete selections
(delete-selection-mode t)
;; disable ring bell
(setq ring-bell-function 'ignore)
;; enable auto revert
(global-auto-revert-mode t)
;; enable abbrev
(abbrev-mode t)

;; enable show parent
(defadvice show-paren-function (around fix-show-paren-function activate)
  (cond ((looking-at-p "\\s(") ad-do-it)
	(t (save-excursion
	     (ignore-errors (backward-up-list))
	     ad-do-it))))

(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)

;; key binding of edit for linux, use <Super> key.
(when (or sys/linuxp sys/linux-x-p)
  ;; select all 
  (bind-key* "s-a" #'mark-whole-buffer)
  ;; save buffer
  (bind-key* "s-s" #'save-buffer)
  ;; copy
  (bind-key* "s-c" #'kill-ring-save)
  ;; paste
  (bind-key* "s-v" #'yank)
  ;; cut
  (bind-key* "s-x" #'kill-region)
  ;; undo
  (bind-key* "s-z" #'undo)

 )

;; Key Modifiers for win32
(when sys/win32p
  ;; make PC keyboard's Win key or other to type Super or Hyper
  ;; (setq w32-pass-lwindow-to-system nil)
  (setq w32-lwindow-modifier 'super)    ; Left Windows key
  (setq w32-apps-modifier 'hyper)       ; Menu/App key

  ;; (w32-register-hot-key [s-])
  (w32-register-hot-key [s-t]))


;; exec path from shell for mac or linux
(when (or sys/mac-x-p sys/linux-x-p)
  (use-package exec-path-from-shell
    :init
    (setq exec-path-from-shell-check-startup-files nil)
    (setq exec-path-from-shell-variables '("PATH" "MANPATH" "PYTHONPATH" "GOPATH"))
    (setq exec-path-from-shell-arguments '("-l"))
    (exec-path-from-shell-initialize)))


;; save place
(use-package saveplace
  :ensure nil
  :init
  ;; Emacs 25 has a proper mode for `save-place'
  (if (fboundp 'save-place-mode)
      (add-hook 'after-init-hook #'save-place-mode)
    (setq save-place t)))

;; recentf
(use-package recentf
  :ensure nil
  :init
  (setq recentf-max-saved-items 200)

  ;; lazy load recentf
  ;; (add-hook 'after-init-hook #'recentf-mode)
  (add-hook 'find-file-hook (lambda () (unless recentf-mode
                                    (recentf-mode)
                                    (recentf-track-opened-file))))
  :config
  (add-to-list 'recentf-exclude (expand-file-name package-user-dir))
  (add-to-list 'recentf-exclude "bookmarks")
  (add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'"))



(provide 'preference-basic)


