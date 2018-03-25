;; mode-popwin.el	-*- lexical-binding: t -*-
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;             popwin.
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

;; popwin
(use-package popwin
    :init 
    :bind 
    (
        ("C-z" . popwin:keymap)
        ("C-<f4>" . popwin:messages)
        ("C-0" . popwin:close-popup-window )
    )

)





(provide 'mode-popwin)

;;----------------------------------------------------------------------------
;; mode-popwin.el ends here
;;----------------------------------------------------------------------------