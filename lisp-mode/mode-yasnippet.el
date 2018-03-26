;; mode-yasnippet.el	-*- lexical-binding: t -*-
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;             Yasnippet.
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

(use-package yasnippet
    :diminish yas-minor-mode
    :init (add-hook 'after-init-hook #'yas-global-mode)
    :config (use-package yasnippet-snippets)
)

(provide 'mode-yasnippet)

;;----------------------------------------------------------------------------
;; mode-yasnippet.el ends here
;;----------------------------------------------------------------------------