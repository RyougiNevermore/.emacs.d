;; mode-evil.el	-*- lexical-binding: t -*-
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;             Evil.
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

;; Evil
(use-package evil
    :init 
    (evil-mode t)
    :config
    (setq evil-want-C-u-scroll t)
    (define-key evil-insert-state-map [escape] 'evil-normal-state)


)

(use-package evil-leader 
    :after evil
    :init
    (global-evil-leader-mode)
    :config   
    (evil-leader/set-leader my-evil-leader-key)
    ;; bind files keys
    (which-key-add-key-based-replacements (concat my-evil-leader-key  " f") "files")
    (evil-leader/set-key
        "ff" 'find-file 
        ;; TODO: ADD MORE KEYS 
        ;; https://github.com/emacs-evil/evil-collection/blob/master/evil-collection.el
        ;; add ivy keys
    )
)

(use-package evil-anzu
    :init
    (global-anzu-mode t)
    :config
    (progn
      (setq anzu-search-threshold 1000
            anzu-cons-mode-line-p nil)
      ;; powerline integration
    (defun my-anzu-update-mode-line (here total)
        "Custom update function which does not propertize the status."
        (when anzu--state
        (let ((status (cl-case anzu--state
                        (search (format "(%s/%d%s)"
                                        (anzu--format-here-position here total)
                                        total (if anzu--overflow-p "+" "")))
                        (replace-query (format "(%d replace)" total))
                        (replace (format "(%d/%d)" here total)))))
            status)))
        (setq anzu-mode-line-update-function 'my-anzu-update-mode-line)
    )
)

(use-package evil-ediff
    :after (ediff)
)

(use-package evil-exchange
    :init (evil-exchange-install)
)

(use-package evil-iedit-state
    :commands (evil-iedit-state evil-iedit-state/iedit-mode)
    :init
    (progn
        (setq iedit-current-symbol-default t
            iedit-only-at-symbol-boundaries t
            iedit-toggle-key-default nil)
            (evil-leader/set-key "se" 'evil-iedit-state/iedit-mode)
    )
    :config
    ;; activate leader in iedit and iedit-insert states
    (define-key evil-iedit-state-map (kbd evil-leader-key) evil-default-map)

)

(use-package evil-indent-plus
    :init (evil-indent-plus-default-bindings)
)

(use-package evil-nerd-commenter
    :commands evilnc-comment-operator
    :init
    (progn
        ;; double all the commenting functions so that the inverse operations
        ;; can be called without setting a flag
        (defun my-evil-comment-or-uncomment-lines-inverse (&optional arg)
            (interactive "p")
            (let ((evilnc-invert-comment-line-by-line t))
            (evilnc-comment-or-uncomment-lines arg))
        )

        (defun  my-evil-comment-or-uncomment-lines (&optional arg)
            (interactive "p")
            (let ((evilnc-invert-comment-line-by-line nil))
            (evilnc-comment-or-uncomment-lines arg))
        )

        (defun  my-evil-copy-and-comment-lines-inverse (&optional arg)
            (interactive "p")
            (let ((evilnc-invert-comment-line-by-line t))
            (evilnc-copy-and-comment-lines arg))
        )

        (defun  my-evil-copy-and-comment-lines (&optional arg)
            (interactive "p")
            (let ((evilnc-invert-comment-line-by-line nil))
            (evilnc-copy-and-comment-lines arg))
        )

        (defun  my-evil-quick-comment-or-uncomment-to-the-line-inverse (&optional arg)
            (interactive "p")
            (let ((evilnc-invert-comment-line-by-line t))
            (evilnc-comment-or-uncomment-to-the-line arg))
        )

        (defun  my-evil-quick-comment-or-uncomment-to-the-line (&optional arg)
            (interactive "p")
            (let ((evilnc-invert-comment-line-by-line nil))
            (evilnc-comment-or-uncomment-to-the-line arg))
        )

        (defun  my-evil-comment-or-uncomment-paragraphs-inverse (&optional arg)
            (interactive "p")
            (let ((evilnc-invert-comment-line-by-line t))
            (evilnc-comment-or-uncomment-paragraphs arg))
        )

        (defun  my-evil-comment-or-uncomment-paragraphs (&optional arg)
            (interactive "p")
            (let ((evilnc-invert-comment-line-by-line nil))
            (evilnc-comment-or-uncomment-paragraphs arg))
        )

        (define-key evil-normal-state-map "gc" 'evilnc-comment-operator)
        (define-key evil-normal-state-map "gy" ' my-evil-copy-and-comment-lines)

        (evil-leader/set-key
        ";"  'evilnc-comment-operator
        "cl" ' my-evil-comment-or-uncomment-lines
        "cL" ' my-evil-comment-or-uncomment-lines-inverse
        "cp" ' my-evil-comment-or-uncomment-paragraphs
        "cP" ' my-evil-comment-or-uncomment-paragraphs-inverse
        "ct" ' my-evil-quick-comment-or-uncomment-to-the-line
        "cT" ' my-evil-quick-comment-or-uncomment-to-the-line-inverse
        "cy" ' my-evil-copy-and-comment-lines
        "cY" ' my-evil-copy-and-comment-lines-inverse
        )
    )    
)

(use-package evil-matchit
    :defer t
)



(provide 'mode-evil)

;;----------------------------------------------------------------------------
;; mode-evil.el ends here
;;----------------------------------------------------------------------------