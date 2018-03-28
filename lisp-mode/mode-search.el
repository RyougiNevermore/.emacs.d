;; mode-Search.el	-*- lexical-binding: t -*-
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;             search.
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

(use-package ag
  :init
  (with-eval-after-load 'projectile
    (bind-key "s s" 'ag-project projectile-command-map))
  :config
  (setq ag-highlight-search t)
  (setq ag-reuse-buffers t)
)

(use-package wgrep-ag
  :config
  (setq wgrep-auto-save-buffer t)
  (setq wgrep-change-readonly-file t)
)

(use-package pt
  :init
  (with-eval-after-load 'projectile
    (bind-key "s p" 'projectile-pt projectile-command-map)
  )
)

(use-package rg
  :init
  (add-hook 'after-init-hook #'rg-enable-default-bindings)
  (if (fboundp 'wgrep-ag-setup)
    (add-hook 'rg-mode-hook #'wgrep-ag-setup)
  )
  :config
  (setq rg-custom-type-aliases nil)
  (setq rg-group-result t)
  (setq rg-show-columns t)

  (with-eval-after-load 'projectile
    (bind-key "s r" 'rg-project projectile-command-map)
  )

  (when (fboundp 'ag)
    (bind-key "a" 'ag rg-global-map)
  )
  (when (fboundp 'pt-regexp)
    (bind-key "P" 'pt-regexp rg-global-map)
  )

  (with-eval-after-load 'counsel
    (bind-key "c r" 'counsel-rg rg-global-map)
    (bind-key "c s" 'counsel-ag rg-global-map)
    (bind-key "c p" 'counsel-pt rg-global-map)
    (bind-key "c f" 'counsel-fzf rg-global-map)
  )

  (with-eval-after-load 'counsel-projectile
    (bind-key "s r" 'rg-project counsel-projectile-command-map)
  )
)


(provide 'mode-search)

;;----------------------------------------------------------------------------
;; mode-search.el ends here
;;----------------------------------------------------------------------------