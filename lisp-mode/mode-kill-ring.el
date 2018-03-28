;; mode-kill-ring.el	-*- lexical-binding: t -*-
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;             Kill Ring.
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

;; (setq kill-ring-max 200)

;; Save clipboard contents into kill-ring before replace them
(setq save-interprogram-paste-before-kill t)

;; Kill & Mark things easily
(use-package easy-kill
    :bind (
                    ([remap kill-ring-save] . easy-kill)
                    ([remap mark-sexp] . easy-mark)
                )
)

;; Interactively insert items from kill-ring
(use-package browse-kill-ring
    :bind ("C-c k" . browse-kill-ring)
    :init (add-hook 'after-init-hook #'browse-kill-ring-default-keybindings)
)

(provide 'mode-kill-ring)

;;----------------------------------------------------------------------------
;; mode-kill-ring.el ends here
;;----------------------------------------------------------------------------