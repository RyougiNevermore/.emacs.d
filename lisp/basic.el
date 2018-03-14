;;----------------------------------------------------------------------------
;; basic
;;----------------------------------------------------------------------------

;; disable tool bar
(tool-bar-mode -1)

;; disable scrool bar
(scroll-bar-mode -1)

;; show line number
(global-linum-mode t)

;; set cursor type 
(setq-default cursor-type 'bar)

;; disable backup files
(setq make-backup-files nil)

;; enable delete selection content
(delete-selection-mode t)

;; enable hight line 
(global-hl-line-mode t)

;; max screen
(setq  initial-frame-alist (quote ((fullscreen . maximized))))

;; disable ring bell
(setq ring-bell-function 'ignore)

;; enable auto revert 
(global-auto-revert-mode t)

;; enable abbrev
(abbrev-mode t)

;; disable auto save
(setq auto-save-default nil)

;; enable show parent
(defadvice show-paren-function (around fix-show-paren-function activate)
  (cond ((looking-at-p "\\s(") ad-do-it)
	(t (save-excursion
	     (ignore-errors (backward-up-list))
	     ad-do-it))))

(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)

(provide 'basic)
