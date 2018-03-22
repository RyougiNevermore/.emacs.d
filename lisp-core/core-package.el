;; core-package.el --- Desfine constants.	-*- lexical-binding: t -*-
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
;;             Packages.
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

(eval-when-compile (require 'core-custom))

;; FIXME: DO NOT copy package-selected-packages to init/custom file forcibly.
;; https://github.com/jwiegley/use-package/issues/383#issuecomment-247801751
(with-eval-after-load 'package
  (defun package--save-selected-packages (&optional value)
    "Set and (don't!) save `package-selected-packages' to VALUE."
    (when value
      (setq package-selected-packages value))
    (unless after-init-time
      (add-hook 'after-init-hook #'package--save-selected-packages))))

;; ELPA: refer to https://elpa.emacs-china.org/
;;
(cond
 ((eq my-package-archives 'melpa)
  (setq package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                           ("melpa" . "https://melpa.org/packages/")
			   ("melpa-stable" . "https://stable.melpa.org/packages/"))))
 ((eq my-package-archives 'emacs-china)
  (setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
                           ("melpa" . "http://elpa.emacs-china.org/melpa/")
			   ("melpa-stable" . "http://elpa.emacs-china.org/melpa-stable/"))))
 ((eq my-package-archives 'tuna)
  (setq package-archives '(("gnu"   . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                           ("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
			   ("melpa-stable" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa-stable/")))))



;; Initialize packages
(setq package-enable-at-startup nil)    ; To prevent initialising twice
(package-initialize)


;; Setup `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq use-package-always-ensure t)
(setq use-package-always-defer t)
(setq use-package-expand-minimally t)
(setq use-package-enable-imenu-support t)

;; Required by `use-package'
(use-package diminish)
(use-package bind-key)

;; Initialization benchmark
(when my-benchmark-enabled
  (use-package benchmark-init
    :init
    (benchmark-init/activate)
    (add-hook 'after-init-hook #'benchmark-init/deactivate)))

;; A mondern package interface
(use-package paradox
  :init (defalias 'upgrade-packages 'paradox-upgrade-packages)
  :config
  (setq paradox-github-token t)
  (setq paradox-execute-asynchronously t)
  (setq paradox-automatically-star nil)
  (setq paradox-display-star-count nil))

(provide 'core-package)


