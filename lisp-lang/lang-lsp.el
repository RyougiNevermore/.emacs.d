;; lang-lsp.el	-*- lexical-binding: t -*-
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;             Lsp.
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


(when (>= emacs-major-version 25)
    ;; Emacs client for the Language Server Protocol
    ;; https://github.com/emacs-lsp/lsp-mode
    (use-package lsp-mode
        :diminish lsp-mode
        :config
        (use-package lsp-ui
            :commands (lsp-ui-mode lsp-ui-peek-find-definistions lsp-ui-peek-find-references)
            :bind (
                        :map lsp-ui-mode-map
                        ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
                        ([remap xref-find-references] . lsp-ui-peek-find-references)
                        )
            :init (add-hook 'lsp-mode-hook 'lsp-ui-mode)
        )

        (with-eval-after-load 'company
            (use-package company-lsp
             :init (cl-pushnew (company-backend-with-yas 'company-lsp) company-backends)
            )
        )
    )

)

(provide 'lang-lsp)

;;----------------------------------------------------------------------------
;; lang-lsp.el ends here
;;----------------------------------------------------------------------------