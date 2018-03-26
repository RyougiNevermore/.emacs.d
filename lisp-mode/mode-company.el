;; mode-company.el	-*- lexical-binding: t -*-
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;             Company.
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

(use-package company
    :diminish company-mode
    :bind (("M-/" . company-complete)
            ("C-c C-y" . company-yasnippet)
            :map company-active-map
            ("C-p" . company-select-previous)
            ("C-n" . company-select-next)
            ;; ("<tab>" . company-complete-selection)
            :map company-search-map
            ("C-p" . company-select-previous)
            ("C-n" . company-select-next))
    :init (add-hook 'after-init-hook #'global-company-mode)
    :config
    ;; aligns annotation to the right hand side
    (setq company-tooltip-align-annotations t)

    (setq company-tooltip-limit 12                      ; bigger popup window
        company-idle-delay .2                         ; decrease delay before autocompletion popup shows
        company-echo-delay 0                          ; remove annoying blinking
        company-minimum-prefix-length 2
        company-require-match nil
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil)

    ;; Popup documentation for completion candidates
    (use-package company-quickhelp
        :if (display-graphic-p)
        :bind (:map company-active-map
                    ("M-h" . company-quickhelp-manual-begin))
        :init (company-quickhelp-mode 1)
        :config (setq company-quickhelp-delay 1)
    )

    ;; Support yas in commpany
    ;; Note: Must be the last to involve all backends
    (defvar company-enable-yas t "Enable yasnippet for all backends.")

    (defun company-backend-with-yas (backend)
        (if (or (not company-enable-yas)
            (and (listp backend) (member 'company-yasnippet backend)))
            backend
            (append (if (consp backend) backend (list backend)) '(:with company-yasnippet))
        )
    )

    (setq company-backends (mapcar #'company-backend-with-yas company-backends))

)

(provide 'mode-company)

;;----------------------------------------------------------------------------
;; mode-company.el ends here
;;----------------------------------------------------------------------------