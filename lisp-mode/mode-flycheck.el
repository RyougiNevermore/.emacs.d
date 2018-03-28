;; mode-flycheck.el	-*- lexical-binding: t -*-
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;             Flycheck.
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


(use-package flycheck
    :diminish flycheck-mode
    :init (add-hook 'after-init-hook #'global-flycheck-mode)
    :config
    (setq flycheck-indication-mode 'right-fringe)
    (setq flycheck-emacs-lisp-load-path 'inherit)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))

    ;; Display Flycheck errors in GUI tooltips
    (use-package flycheck-pos-tip
        :init (flycheck-pos-tip-mode 1)
        :config (setq flycheck-pos-tip-timeout 15)
    )

    ;; Jump to and fix syntax errors via `avy'
    (use-package avy-flycheck
        :init (avy-flycheck-setup)
    )

)

(provide 'mode-flycheck)

;;----------------------------------------------------------------------------
;; mode-flycheck.el ends here
;;----------------------------------------------------------------------------