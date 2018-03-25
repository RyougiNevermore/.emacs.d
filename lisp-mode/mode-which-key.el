;; mode-which-key.el	-*- lexical-binding: t -*-
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;             Which key.
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


;; Display available keybindings in popup
(use-package which-key
  :diminish which-key-mode
  :bind (:map help-map ("C-h" . which-key-C-h-dispatch))
  :init (add-hook 'after-init-hook #'which-key-mode)
  :config
  (setq which-key-allow-evil-operators t)
  (setq which-key-show-operator-state-maps t)
  (which-key-setup-side-window-bottom)
  (which-key-setup-minibuffer)
  (setq which-key-idle-delay 0.5)

)



(provide 'mode-which-key)

;;----------------------------------------------------------------------------
;; mode-evil.el ends here
;;----------------------------------------------------------------------------