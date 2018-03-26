;; core-modeline.el	-*- lexical-binding: t -*-
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;             Mode line.
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

;; spaceline
(use-package spaceline-config
  :ensure spaceline
  :commands (spaceline-spacemacs-theme spaceline-info-mode)
  :init 
  (add-hook 'after-init-hook #'spaceline-spacemacs-theme)
  ;;(add-hook 'after-init-hook #'spaceline-info-mode)
  :config
  (setq spaceline-pre-hook #'powerline-reset) ; Fix for changing themes
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-modified)
  (setq powerline-default-separator (if window-system 'arrow 'utf-8))
  (setq powerline-image-apple-rgb sys/mac-x-p)
  
)

(use-package spaceline-all-the-icons 
  :after spaceline-config
  :config 
  (spaceline-all-the-icons-theme)
  (spaceline-all-the-icons--setup-anzu)            ;; Enable anzu searching
  (spaceline-all-the-icons--setup-package-updates) ;; Enable package update indicator
  (spaceline-all-the-icons--setup-git-ahead)       ;; Enable # of commits ahead of upstream in git
  (spaceline-all-the-icons--setup-neotree)         ;; Enable Neotree mode line
  ;;:custom
  ;;(spaceline-all-the-icons-icon-set-modified 'toggle "")
  ;;(spaceline-all-the-icons-icon-set-window-numbering 'circle "")
  
)

(provide 'core-modeline)
;;----------------------------------------------------------------------------
;; core-modeline.el ends here
;;----------------------------------------------------------------------------
