;; lang-go.el	-*- lexical-binding: t -*-
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;             Golang.
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


;; Golang
;;
;; Go packages:
;; go get -u github.com/nsf/gocode
;; go get -u github.com/rogpeppe/godef
;; go get -u github.com/jstemmer/gotags
;; go get -u github.com/golang/lint/golint
;; go get -u golang.org/x/tools/cmd/goimports
;; go get -u golang.org/x/tools/cmd/guru
;; go get -u golang.org/x/tools/cmd/gorename
;; go get -u golang.org/x/tools/cmd/godoc
;; go get -u github.com/derekparker/delve/cmd/dlv
;; go get -u github.com/josharian/impl
;; go get -u github.com/cweill/gotests/...
;; go get -u github.com/fatih/gomodifytags
;; go get -u github.com/davidrjenni/reftools/cmd/fillstruct
;;
(use-package go-mode
    :bind (
                :map go-mode-map
                ("C-c C-i" . go-import-add)
                ("C-c C-r" . go-remove-unused-imports)
                ("C-c C-d" . godef-describe)
                ("M-." . godef-jump-other-window)
                ("C-c C-f a" . go-goto-arguments)
                ("C-c C-f d" . go-goto-docstring)
                ("C-c C-f f" . go-goto-function)
                ("C-c C-f n" . go-goto-function-name)
                ("C-c C-f r" . go-goto-return-values)
                ("C-c C-f m" . go-goto-method-receiver)
                ("<f1>" . godoc-at-point)
                )
    :config
    ;; `goimports' or `gofmt'
    (setq gofmt-command "goimports")
    (add-hook 'before-save-hook #'gofmt-before-save)

    (use-package go-dlv)
    (use-package go-fill-struct)
    (use-package go-impl)
    (use-package go-playground)
    (use-package golint)
    (use-package govet)

    (use-package go-eldoc
        :init (add-hook 'go-mode-hook #'go-eldoc-setup)
    )

    (use-package go-guru
        :bind (
                    :map go-mode-map("M-?" . go-guru-referrers)
                     )
    )

    (use-package go-tag
        :bind (
                    :map go-mode-map
                    ("C-c T a" . go-tag-add)
                    ("C-c T d" . go-tag-remove)
                    )
    )

    (use-package go-direx
        :bind (
                    :map go-mode-map
                    ("C-c C-j" . go-direx-pop-to-buffer)

                    )
    )

    (use-package gotest
        :bind (
                    :map go-mode-map
                    ("C-c a" . go-test-current-project)
                    ("C-c m" . go-test-current-file)
                    ("C-c ." . go-test-current-test)
                    ("C-c x" . go-run)
                    )
    )

    (use-package go-gen-test
        :bind (
                    :map go-mode-map
                    ("C-c C-g" . go-gen-test-dwim)
                    )
    )

    (with-eval-after-load 'company
        (use-package company-go
            :defer t
            :init 
            (progn
                (setq company-go-show-annotation t)
                (cl-pushnew (company-backend-with-yas 'company-go) company-backends)
            )
        )
    )

    (with-eval-after-load 'projectile
        ;; M-x `go-projectile-install-tools'
        (use-package go-projectile
        :commands (go-projectile-mode go-projectile-switch-project)
        :init
        (add-hook 'projectile-after-switch-project-hook #'go-projectile-switch-project)
        (add-hook 'go-mode-hook #'go-projectile-mode))
    )

    (with-eval-after-load 'evil-leader 
        ;; evil keys
        (which-key-add-key-based-replacements (concat my-evil-leader-key  " g") "Golang")
        (which-key-add-key-based-replacements (concat my-evil-leader-key  " gi") "Golang import")
        (which-key-add-key-based-replacements (concat my-evil-leader-key  " gg") "Golang goto")
        (which-key-add-key-based-replacements (concat my-evil-leader-key  " gt") "Golang tag")
        (evil-leader/set-key
            "go" 'go-impl
            "gd" 'godoc-at-point
            "gia" 'go-import-add
            "gic" 'go-remove-unused-imports
            "gj" 'godef-jump-other-window
            "gga" 'go-goto-arguments
            "ggd" 'go-goto-docstring
            "ggf" 'go-goto-function
            "ggn" 'go-goto-function-name
            "ggr" 'go-goto-return-values
            "ggm" 'go-goto-method-receiver
            "gta" 'go-tag-add
            "gtr" 'go-tag-remove
        )
    )

)

(provide 'lang-go)

;;----------------------------------------------------------------------------
;; lang-go.el ends here
;;----------------------------------------------------------------------------