;; mode-window.el	-*- lexical-binding: t -*-
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;             Window.
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

;; Numbered window shortcuts
(use-package window-numbering
  :init 
  (add-hook 'after-init-hook #'window-numbering-mode)
)

;; Popup Window Manager
(use-package popwin
    :commands popwin-mode
    :init (add-hook 'after-init-hook #'popwin-mode)
    :bind 
    (
        ("C-z" .  popwin:keymap)
        ("C-<f4>" . popwin:messages)
        ("C-0" . popwin:close-popup-window )
    )
    :config
    ;; don't use default value but manage it ourselves
    (setq popwin:special-display-config
        '(;; Emacs
            ("*Help*" :dedicated t :position bottom :stick nil :noselect nil)
            ("*compilation*" :dedicated t :position bottom :stick t :noselect t :height 0.4)
            ("*Compile-Log*" :dedicated t :position bottom :stick t :noselect t :height 0.4)
            ("*Warnings*" :dedicated t :position bottom :stick t :noselect t)
            ("*Completions*" :dedicated t :position bottom :stick t :noselect nil)
            ("*Shell Command Output*" :dedicated t :position bottom :stick t :noselect nil)
            ("\*Async Shell Command\*.+" :regexp t :position bottom :stick t :noselect nil)
            ("^*Man.+*$" :regexp t :position bottom :stick nil :noselect nil :height 0.4)
            ("^*WoMan.+*$" :regexp t :position bottom)
            ("^*Backtrace.+*$" :regexp t :dedicated t :position bottom :stick t :noselect nil)

            ;; Kill Ring
            ("*Kill Ring*" :dedicated t :position bottom)

            ;; Flycheck
            ("\*flycheck errors\*.+*$" :regexp t :position bottom :stick t :noselect nil)

            ;; Youdao dict
            ;;("*Youdao Dictionary*" :dedicated t :position bottom)
            
            ;; List
            ("*Colors*" :dedicated t :position bottom)
            ("*Process List*" :dedicated t :position bottom)
            ("*Process-Environment*" :dedicated t :position bottom)

            ;; undo-tree
            (" *undo-tree*" :dedicated t :position right :stick t :noselect nil :width 60)

            ;; Search
            ("*grep*" :dedicated t :position bottom :stick t :noselect nil)
            ("*ag search*" :dedicated t :position bottom :stick t :noselect nil :height 0.4)
            ("*rg*" :dedicated t :position bottom :stick t :noselect nil :height 0.4)
            ("*pt-search*" :dedicated t :position bottom :stick t :noselect nil :height 0.4)
            ("*Occur*" :dedicated t :position bottom :stick t :noselect nil)
            ("\*ivy-occur.+*$" :regexp t :position bottom :stick t :noselect nil)
            ("*xref*" :dedicated t :position bottom :stick nil :noselect nil)

            ;; VC
            ("*vc-diff*" :dedicated t :position bottom :stick t :noselect nil)
            ("*vc-change-log*" :dedicated t :position bottom :stick t :noselect nil)

            ;; Magit
            ;; (magit-status-mode :dedicated t :position bottom :stick t :height 0.5)
            ;; (magit-diff-mode :dedicated t :position bottom :stick t :noselect t :height 0.5)

            ;; Script
            ("*shell*" :dedicated t :position bottom :stick t :noselect nil)
            ("*Python*" :dedicated t :position bottom :stick t :noselect t)
            ("*Ruby*" :dedicated t :position bottom :stick t :noselect t)
            ("*quickrun*" :dedicated t :position bottom :stick t :noselect t)

            ;; Go
            ("^*godoc.+*$" :regexp t :position bottom :stick nil :noselect nil)
            ("*golint*" :dedicated t :position bottom :stick t :noselect nil)
            ("*govet*" :dedicated t :position bottom :stick t :noselect nil)
            ("*go-guru-output*" :dedicated t :position bottom :stick t :noselect nil)
            ("*Gofmt Errors*" :dedicated t :position bottom :stick t :noselect nil)
            ("*Go Test*" :dedicated t :position bottom :stick t :noselect nil)

            ;; Test
            ("*ert*" :dedicated t :position bottom :stick t :noselect nil)
            ("*nosetests*" :dedicated t :position bottom :stick t :noselect nil)
        )
    )
)

(provide 'mode-window)

;;----------------------------------------------------------------------------
;; mode-window.el ends here
;;----------------------------------------------------------------------------