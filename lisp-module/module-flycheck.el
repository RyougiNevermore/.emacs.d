;; module-flycheck.el --- Desfine constants.	-*- lexical-binding: t -*-
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
;;             Flycheck configurations.
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


(use-package flycheck
  :diminish flycheck-mode
  :init (add-hook 'after-init-hook #'global-flycheck-mode)
  :config
  (setq flycheck-indication-mode 'right-fringe)
  (setq flycheck-emacs-lisp-load-path 'inherit)

  ;; Display Flycheck errors in GUI tooltips
  (use-package flycheck-pos-tip
    :init (flycheck-pos-tip-mode 1)
    :config (setq flycheck-pos-tip-timeout 15))

  ;; Jump to and fix syntax errors via `avy'
  (use-package avy-flycheck
    :init (avy-flycheck-setup)))

(provide 'module-flycheck)
