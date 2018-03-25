;; mode-evil.el	-*- lexical-binding: t -*-
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;             Evil.
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

;; Evil
(use-package evil
    :init 
    (evil-mode t)
    :config
    
    (setq evil-want-C-u-scroll t)
    (define-key evil-insert-state-map [escape] 'evil-normal-state)


)



(use-package evil-leader 
    :after evil
    :init
    (global-evil-leader-mode)
    :config   
    (evil-leader/set-leader "<SPC>")
    (evil-leader/set-key
        "ff" 'find-file
        ;; TODO: ADD MORE KEYS 
        ;; https://github.com/emacs-evil/evil-collection/blob/master/evil-collection.el
        ;; add ivy keys
    )
)






(provide 'mode-evil)

;;----------------------------------------------------------------------------
;; mode-evil.el ends here
;;----------------------------------------------------------------------------