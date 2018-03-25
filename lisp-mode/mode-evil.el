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
        ;;(evil-leader-mode)
        
    (evil-leader/set-leader "<SPC>")
    (evil-leader/set-key
        "ff" 'find-file
    )
)






(provide 'mode-evil)

;;----------------------------------------------------------------------------
;; mode-evil.el ends here
;;----------------------------------------------------------------------------