;;; user-init.el  -*- no-byte-compile: t -*-
;;; Commentary:
;;;       Edit user-init.el and change the configurations,
;;;       then restart Emacs.
;;; Code:

;;----------------------------------------------------------------------------
;; User settings
;;----------------------------------------------------------------------------

;; User full name
(setq my-full-name "ryougi")
;; Email address
(setq my-mail-address "ryougi.nevermore@hotmail.com")
;; Package repo: melpa (default), emacs-china or tuna
(setq my-package-archives 'emacs-china)
;; Set evil leader key. default is <SPC>
(setq my-evil-leader-key "<SPC>")
;; Enable emoji: t or nil
(setq my-emoji-enabled t)
;; Enable initialization benchmark: t or nil
(setq my-benchmark-enabled nil)
;; Org agenda dir 
(setq my-org-agenda-dir "~/Org")
;; frame: t or nil
;; (setq my-frame-max-screen-enabled t)
;; Use osx key mode. : t or nil
;;(setq osx-key-mode t)
;; Make Super key be CUA leader key. : t or nil
(setq cua-super-key t)
;; Setup Meta(Alt) and Super(Win) key for windows(Cygwin). : t or nil
;; (setq win-meta-super-key t)
;; Theme (default, dracula, spacemacs-dark, spacemacs-light, and spacemacs-daylight)
(setq my-theme 'dracula)

;;----------------------------------------------------------------------------
;; user-init.el ends here
;;----------------------------------------------------------------------------
