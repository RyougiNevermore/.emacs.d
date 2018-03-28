;; mode-prog.el	-*- lexical-binding: t -*-
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;             Prog.
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


(use-package prog-mode
    :ensure nil
    :init
    ;; Prettify Symbols
    ;; e.g. display “lambda” as “λ”
    (when (boundp 'global-prettify-symbols-mode)
        (add-hook 'after-init-hook #'global-prettify-symbols-mode)
        (add-hook 'emacs-lisp-mode-hook
                    (lambda ()
                    (push '("<=" . ?≤) prettify-symbols-alist))
        )
    )
)

;; Jump to definition via `ag'/`rg'/`grep'
(use-package dumb-jump
    :bind (
                    ("M-g o" . dumb-jump-go-other-window)
                    ("M-g j" . dumb-jump-go)
                    ("M-g i" . dumb-jump-go-prompt)
                    ("M-g x" . dumb-jump-go-prefer-external)
                    ("M-g z" . dumb-jump-go-prefer-external-other-window)
                )
    :init (add-hook 'after-init-hook #'dumb-jump-mode)
    :config
    (setq dumb-jump-prefer-searcher 'rg)
    (with-eval-after-load 'ivy
    (setq dumb-jump-selector 'ivy))

    (with-eval-after-load 'hydra
        (defhydra dumb-jump-hydra (:color blue :columns 3) 
            "Dumb Jump"
            ("j" dumb-jump-go "Go")
            ("o" dumb-jump-go-other-window "Other window")
            ("e" dumb-jump-go-prefer-external "Go external")
            ("x" dumb-jump-go-prefer-external-other-window "Go external other window")
            ("i" dumb-jump-go-prompt "Prompt")
            ("l" dumb-jump-quick-look "Quick look")
            ("b" dumb-jump-back "Back")
        )
        (bind-key "C-M-j" #'dumb-jump-hydra/body dumb-jump-mode-map)
    )
)

(use-package nxml-mode
    :ensure nil
    :mode (("\\.xaml$" . xml-mode))
)

(use-package quickrun
    :bind (("<f7>" . quickrun)
                ("C-c x" . quickrun))
)

(use-package dockerfile-mode :mode "Dockerfile\\'")
(use-package vimrc-mode)

;; New `conf-toml-mode' in Emacs26
(unless (fboundp 'conf-toml-mode)
    (use-package toml-mode)
)

(use-package editorconfig
    :diminish editorconfig-mode
    :init (add-hook 'after-init-hook #'editorconfig-mode)
)

(provide 'mode-prog)

;;----------------------------------------------------------------------------
;; mode-prog.el ends here
;;----------------------------------------------------------------------------