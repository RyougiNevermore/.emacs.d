;; core-util.el	-*- lexical-binding: t -*-
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;             Utils.
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

;; Page break lines
(use-package page-break-lines
    :config
    (global-page-break-lines-mode)
    (set-fontset-font "fontset-default"
                  (cons page-break-lines-char page-break-lines-char)
                  (face-attribute 'default :family))
)

;; Youdao Dictionay
(use-package youdao-dictionary
  :bind (
              ("C-c y" . youdao-dictionary-search-at-point)
              ("C-c Y" . youdao-dictionary-search-at-point-tooltip)
              )
  :config
  ;; Cache documents
  (setq url-automatic-caching t)

  ;; Enable Chinese word segmentation support (支持中文分词)
  (setq youdao-dictionary-use-chinese-word-segmentation t)

)

;; Emoji
(when my-emoji-enabled
  (use-package emojify
    :init (add-hook 'after-init-hook #'global-emojify-mode)
    :config
    (with-eval-after-load 'company
      (use-package company-emoji
        :init (add-to-list 'company-backends 'company-emoji))
    )
  )
)

;; Discover key bindings and their meaning for the current Emacs major mode
(use-package discover-my-major
  :bind (
              ("C-h M-m" . discover-my-major)
              ("C-h M-M" . discover-my-mode)
              )
)

;; A Simmple and cool pomodoro timer
(use-package pomidor
  :bind (("<f12>" . pomidor))
)

;; Misc
(use-package copyit)                    ; copy path, url, etc.
(use-package diffview)                  ; side-by-side diff view
(use-package esup)                      ; Emacs startup profiler
(use-package fontawesome)
(use-package htmlize)                   ; covert to html
(use-package list-environment)
(use-package memory-usage)
(use-package open-junk-file)
(use-package try)
(use-package ztree)                     ; text mode directory tree. Similar with beyond compare



(provide 'core-util)

;;----------------------------------------------------------------------------
;; core-util.el ends here
;;----------------------------------------------------------------------------