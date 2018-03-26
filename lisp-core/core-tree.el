;; core-tree.el	-*- lexical-binding: t -*-
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;             Tree.
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

(use-package neotree
    :defer t
    :commands neo-global--window-exists-p
    :init
    (progn
        (setq neo-window-width 32
            neo-create-file-auto-open t
            neo-banner-message "Press ? for neotree help"
            neo-show-updir-line nil
            neo-mode-line-type 'neotree
            neo-smart-open t
            neo-dont-be-alone t
            neo-persist-show nil
            neo-show-hidden-files t
            neo-auto-indent-point t
            neo-modern-sidebar t
            neo-vc-integration nil
        )
        
    )
    :config
    (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
    :bind
    ([f8] . neotree-toggle)

)


(provide 'core-tree)

;;----------------------------------------------------------------------------
;; core-tree.el ends here
;;----------------------------------------------------------------------------