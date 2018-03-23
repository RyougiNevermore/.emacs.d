;; module-go.el --- Desfine constants.	-*- lexical-binding: t -*-
;;
;; Author: Ryougi Nevermore <ryougi.nevermore@hotmail.com>
;; Version: 0.0.1
;; URL: https://github.com/RyougiNevermore/.emacs.d
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;             Golang configurations.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:


;; Golang
;;
;; Go packages:
;; go get -u github.com/nsf/gocode
;; go get -u github.com/rogpeppe/godef
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
  :bind (:map go-mode-map
              ("C-c C-j" . godef-jump)
              ("C-x 4 C-c C-j" . godef-jump-other-window)

              ("C-c C-a" . go-import-add)
              ("C-c C-r" . go-remove-unused-imports)

              ("C-c C-f f" . go-goto-function)
              ("C-c C-f n" . go-goto-function-name)
              ("C-c C-f r" . go-goto-return-values)
              ("C-c C-f m" . go-goto-method-receiver)
              
              ("<f1>" . godoc-at-point))
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
    :init (add-hook 'go-mode-hook #'go-eldoc-setup))

  (use-package go-guru
    :bind (:map go-mode-map
                ("M-?" . go-guru-referrers)))

  (use-package go-tag
    :bind (:map go-mode-map
                ("C-c C-t a" . go-tag-add)
                ("C-c C-t r" . go-tag-remove)))


  (use-package go-errcheck
    :bind (:map go-mode-map 
                ("C-c e" . go-errcheck)))

  (use-package gotest
    :bind (:map go-mode-map
                ("C-c a" . go-test-current-project)
                ("C-c m" . go-test-current-file)
                ("C-c ." . go-test-current-test)
                ("C-c x" . go-run)))

  (use-package go-gen-test
    :bind (:map go-mode-map
                ("C-c C-g" . go-gen-test-dwim)))

  (with-eval-after-load 'company
    (use-package company-go
      :init (cl-pushnew (company-backend-with-yas 'company-go) company-backends)
    ))

  (with-eval-after-load 'projectile
    ;; M-x `go-projectile-install-tools'
    (use-package go-projectile
      :commands (go-projectile-mode go-projectile-switch-project)
      :init
      (add-hook 'projectile-after-switch-project-hook #'go-projectile-switch-project)
      (add-hook 'go-mode-hook #'go-projectile-mode))))


;;(with-eval-after-load 'go-mode
;;    (use-package flymake-go))

(provide 'module-go)