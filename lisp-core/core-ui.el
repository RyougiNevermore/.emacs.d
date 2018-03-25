;; core-ui.el	-*- lexical-binding: t -*-
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;             User Interface.
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

  ;; icon
(use-package all-the-icons)

;; theme
(use-package challenger-deep-theme
    :init (load-theme 'challenger-deep t)
    :config
    (custom-set-faces 
      `(font-lock-function-name-face ((t (:bold, t))) t) 
      `(font-lock-keyword-face ((t (:bold, t))) t) 
      `(font-lock-builtin-face ((t (:bold, t))) t) 
      `(font-lock-type-face ((t (:bold, t))) t)))

;; spaceline
(use-package spaceline-config
  :ensure spaceline
  :commands spaceline-spacemacs-theme
  :init (add-hook 'after-init-hook #'spaceline-spacemacs-theme)
  :config
  (setq spaceline-pre-hook #'powerline-reset) ; Fix for changing themes
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  (setq powerline-default-separator (if window-system 'arrow 'utf-8))
  (setq powerline-image-apple-rgb sys/mac-x-p)
  (setq spaceline-workspace-numbers-unicode t)
  (spaceline-info-mode t)
  (spaceline-toggle-window-number-on)
  (spaceline-toggle-evil-state-on)
  (spaceline-toggle-version-control-on)
  (spaceline-toggle-version-control-on)
  

  
)

(use-package spaceline-all-the-icons 
  :after spaceline
  :config 
  (spaceline-all-the-icons-theme)
  (spaceline-all-the-icons--setup-anzu)            ;; Enable anzu searching
  (spaceline-all-the-icons--setup-package-updates) ;; Enable package update indicator
  (spaceline-all-the-icons--setup-git-ahead)       ;; Enable # of commits ahead of upstream in git
  (spaceline-all-the-icons--setup-paradox)         ;; Enable Paradox mode line
  (spaceline-all-the-icons--setup-neotree)         ;; Enable Neotree mode line
  (setq spaceline-all-the-icons-icon-set-modified 'toggle)
  (setq spaceline-all-the-icons-icon-set-window-numbering 'circle)
  
)

;; tree TODO:


(provide 'core-ui)
;;----------------------------------------------------------------------------
;; core-ui.el ends here
;;----------------------------------------------------------------------------
