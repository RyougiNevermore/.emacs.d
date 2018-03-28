;; mode-ibuffer.el	-*- lexical-binding: t -*-
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;             Ibuffer.
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

(use-package ibuffer-projectile
    :bind ("C-x C-b" . ibuffer)
    :init
    (setq ibuffer-filter-group-name-face 'font-lock-function-name-face)
    (add-hook 'ibuffer-hook
        (lambda ()
            (ibuffer-auto-mode 1)
            (ibuffer-projectile-set-filter-groups)
            (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic)
            )
        )
    )
)


(provide 'mode-ibuffer)

;;----------------------------------------------------------------------------
;; mode-ibuffer.el ends here
;;----------------------------------------------------------------------------