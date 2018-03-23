;; module-c.el --- Desfine constants.	-*- lexical-binding: t -*-
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
;;             C/CPP configurations.
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

;; C/C++ Mode
(use-package cc-mode
  :ensure nil
  :init
  (add-hook 'c-mode-common-hook
            (lambda ()
              (c-set-style "bsd")
              (setq tab-width 4)
              (setq c-basic-offset 4)

              ;; (local-set-key "\C-m" 'reindent-then-newline-and-indent)
              (local-set-key "\C-cc" 'compile)))
  :config
  ;; Company mode backend for C/C++ header files
  (with-eval-after-load 'company
    (use-package company-c-headers
      :init (cl-pushnew (company-backend-with-yas 'company-c-headers) company-backends))))

(provide 'module-c)