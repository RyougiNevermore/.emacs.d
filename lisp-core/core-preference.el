;; core-preference.el	-*- lexical-binding: t -*-
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
  (require 'core-package)
)

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
;; Show path if names are same
(setq uniquify-buffer-name-style 'post-forward-angle-brackets) 
(setq adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*")
(setq adaptive-fill-first-line-regexp "^* *$")
;; Repeating C-SPC after popping mark pops it again
(setq set-mark-command-repeat-pop t)

;; Menu/Tool/Scroll bars
(unless (or sys/mac-x-p sys/linux-x-p) (menu-bar-mode -1))
(and (bound-and-true-p tool-bar-mode) (tool-bar-mode -1))
(and (bound-and-true-p scroll-bar-mode) (scroll-bar-mode -1))
(and (bound-and-true-p horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1))

;; show line number
(global-linum-mode t)
;; set cursor type
(setq-default cursor-type 'bar)
;; max screen
(when my-frame-max-screen-enabled 
  (setq  initial-frame-alist (quote ((fullscreen . maximized))))
)

;; Line and Column
(setq-default fill-column 80)
(setq column-number-mode t)
(setq line-number-mode t)

;; Theme
(cond
 ((eq my-theme 'default)
  (use-package challenger-deep-theme
    :init (load-theme 'challenger-deep t)
  ))
  ((eq my-theme 'dracula)
  (use-package dracula-theme
    :init (load-theme 'dracula t)
  ))
 ((eq my-theme 'spacemacs-dark)
  (use-package spacemacs-theme
    :init (load-theme 'spacemacs-dark t)
  ))
 ((eq my-theme 'spacemacs-light)
  (use-package spacemacs-theme
    :init (load-theme 'spacemacs-light t)
  ))
 ((eq my-theme 'spacemacs-daylight)
  (use-package leuven-theme
    :init (load-theme 'leuven t)
  ))
)

;; Meta and Super Key for windows 
(when (and sys/cygwinp win-meta-super-key) 
  ;; Left Windows key
  (setq w32-pass-lwindow-to-system nil)
  (setq w32-lwindow-modifier 'super)
  ;; Right Windows key
  (setq w32-pass-rwindow-to-system nil)
  (setq w32-rwindow-modifier 'super)
  ;; Menu/App key
  (setq w32-pass-apps-to-system nil)
  (setq w32-apps-modifier 'hyper)
  (w32-register-hot-key [s-t])
)

;; osx key
(when (and sys/mac-x-p osx-key-mode)
  ;; make opt key do Super
  (setq mac-command-modifier 'super)
  ;; make cmd key do Meta 
  (setq mac-option-modifier 'meta) 
  ;; make Control key do Control
  (setq mac-control-modifier 'control)
  ;; make Fn key do Hyper 
  (setq ns-function-modifier 'hyper)    
)

;; History
(use-package saveplace
  :ensure nil
  :init
  ;; Emacs 25 has a proper mode for `save-place'
  (if (fboundp 'save-place-mode)
    (add-hook 'after-init-hook #'save-place-mode)
    (setq save-place t)
  )
)

;; icon
 ;; after install -> M-x all-the-icons-install-fonts
(use-package all-the-icons)

;; exec path from shell for mac or linux
(when (or (or sys/mac-x-p sys/linux-x-p) sys/cygwinp)
  (use-package exec-path-from-shell
    :init
    (setq exec-path-from-shell-check-startup-files nil)
    (setq exec-path-from-shell-variables '("PATH" "MANPATH" "PYTHONPATH" "GOPATH"))
    (exec-path-from-shell-initialize)
  )
)

;; restart-emacs
(use-package restart-emacs
  :config
)

(provide 'core-preference)

;;----------------------------------------------------------------------------
;; core-preference.el ends here
;;----------------------------------------------------------------------------