;; core-edit.el	-*- lexical-binding: t -*-
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
  (require 'core-package)
)

;; Explicitly set the prefered coding systems to avoid annoying prompt
;; from emacs (especially on Microsoft Windows)
(prefer-coding-system 'utf-8)

;; Kill line including '\n'
(setq-default kill-whole-line t)

(defun my-backward-kill-word-or-region (&optional arg)
  "Calls `kill-region' when a region is active and
`backward-kill-word' otherwise. ARG is passed to
`backward-kill-word' if no region is active."
  (interactive "p")
  (if (region-active-p)
    (call-interactively #'kill-region)
    (backward-kill-word arg)
  )
)

;; key binding of cua, use <Super> key.
(when (and cua-super-key (not sys/mac-x-p))
  ;; select all 
  (bind-key* "s-a" 'mark-whole-buffer)
  ;; save buffer
  (bind-key* "s-s" 'save-buffer)
  ;; copy
  (bind-key* "s-c" 'kill-ring-save)
  ;; paste
  (bind-key* "s-v" 'yank)
  ;; cut
  (bind-key* "s-x" 'kill-region)
  ;; undo
  (bind-key* "s-z" 'undo)
  ;; backward-kill-word
  (bind-key* "C-w" 'my-backward-kill-word-or-region)
  ;; 'kill-whole-line
  (bind-key* "s-d" 'kill-whole-line)
 )

 ;; Rectangle
(use-package rect
  :ensure nil
  :bind (("<C-return>" . rectangle-mark-mode))
)

;; Automatically reload files was modified by external program
(use-package autorevert
  :ensure nil
  :diminish auto-revert-mode
  :init (add-hook 'after-init-hook #'global-auto-revert-mode)
)

;; Delete selection if you insert
(use-package delsel
  :ensure nil
  :init (add-hook 'after-init-hook #'delete-selection-mode)
)

  ;; Hungry deletion
(use-package hungry-delete
  :diminish hungry-delete-mode
  :init (add-hook 'after-init-hook #'global-hungry-delete-mode)
  :config (setq-default hungry-delete-chars-to-skip " \t\f\v")
)

;; Move to the beginning/end of line or code
(use-package mwim
    :defer t
    :init
    (progn
      (global-set-key (kbd "C-a") 'mwim-beginning)
      (global-set-key (kbd "C-e") 'mwim-end)
    )
)

;; Treat undo history as a tree
(use-package undo-tree
  :diminish undo-tree-mode
  :init (add-hook 'after-init-hook #'global-undo-tree-mode)
)

;; Handling capitalized subwords in a nomenclature
(use-package subword
  :ensure nil
  :diminish subword-mode
  :init
  (add-hook 'prog-mode-hook #'subword-mode)
  (add-hook 'minibuffer-setup-hook #'subword-mode)
)

;; On-the-fly spell checker
(use-package flyspell
  :ensure nil
  :diminish flyspell-mode
  :init (setq flyspell-issue-message-flag nil)
)

  ;; Automatic parenthesis pairing
(use-package elec-pair
  :ensure nil
  :init (add-hook 'after-init-hook #'electric-pair-mode)
)

;; Increase selected region by semantic units
(use-package expand-region
  :bind ("C-=" . er/expand-region)
)

;; An all-in-one comment command to rule them all
(use-package comment-dwim-2
  :bind ("M-/" . comment-dwim-2)
  :config
  (setq comment-dwim-2--inline-comment-behavior 'reindent-comment)
)

;; comment tags
(use-package comment-tags
  :defer t
  :init
  (comment-tags-mode)
  (autoload 'comment-tags-mode "comment-tags-mode")
  (setq comment-tags-keymap-prefix (kbd "C-c t"))
  (with-eval-after-load "comment-tags"
    (setq comment-tags-keyword-faces
          `(("TODO" . ,(list :weight 'bold :foreground "#28ABE3"))
            ("FIXME" . ,(list :weight 'bold :foreground "#DB3340"))
            ("BUG" . ,(list :weight 'bold :foreground "#DB3340"))
            ("HACK" . ,(list :weight 'bold :foreground "#E8B71A"))
            ("KLUDGE" . ,(list :weight 'bold :foreground "#E8B71A"))
            ("XXX" . ,(list :weight 'bold :foreground "#F7EAC8"))
            ("INFO" . ,(list :weight 'bold :foreground "#F7EAC8"))
            ("DONE" . ,(list :weight 'bold :foreground "#1FDA9A"))))
    (setq comment-tags-comment-start-only t
          comment-tags-require-colon t
          comment-tags-case-sensitive t
          comment-tags-show-faces t
          comment-tags-lighter nil)
  )
  (add-hook 'prog-mode-hook 'comment-tags-mode)
  :catch 
  (lambda (keyword err)
    (message (error-message-string err))
  )
)

;; Drag stuff (lines, words, region, etc...) around
(use-package drag-stuff
  :diminish drag-stuff-mode
  :init (add-hook 'after-init-hook #'drag-stuff-global-mode)
  :config
  (add-to-list 'drag-stuff-except-modes 'org-mode)
  (drag-stuff-define-keys)
)

;; A comprehensive visual interface to diff & patch
(use-package ediff
  :ensure nil
  :init
  ;; show org ediffs unfolded
  (with-eval-after-load 'outline
    (add-hook 'ediff-prepare-buffer-hook #'show-all)
  )
  ;; restore window layout when done
  (with-eval-after-load 'winner
    (add-hook 'ediff-quit-hook #'winner-undo)
  )
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-merge-split-window-function 'split-window-horizontally)
)

;; Show number of matches in mode-line while searching
(use-package anzu
  :diminish anzu-mode
  :bind (
                ([remap query-replace] . anzu-query-replace)
                ([remap query-replace-regexp] . anzu-query-replace-regexp)
                :map isearch-mode-map
                ([remap isearch-query-replace] . anzu-isearch-query-replace)
                ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp)
              )
  :init (add-hook 'after-init-hook #'global-anzu-mode)
  :config 
  (setq anzu-replace-to-string-separator
    (if (char-displayable-p ?→) " → " " -> ")
  )
)

;; unfill M-q
(use-package unfill
  :defer t
  :commands (unfill-region unfill-paragraph unfill-toggle)
  :init
  (global-set-key [remap fill-paragraph] #'unfill-toggle)
)

;; Framework for mode-specific buffer indexes
(use-package imenu
  :ensure nil
  :bind (("C-." . imenu))
)

;; Edit multiple regions in the same way simultaneously
(use-package iedit
  :bind (("C-;" . iedit-mode)
         ("C-x r RET" . iedit-rectangle-mode)
         :map isearch-mode-map ("C-;" . iedit-mode-from-isearch)
         :map esc-map ("C-;" . iedit-execute-last-modification)
         :map help-map ("C-;" . iedit-mode-toggle-on-function))
  :config
  ;; Avoid restoring `iedit-mode'
  (with-eval-after-load 'desktop
    (add-to-list 'desktop-minor-mode-table  '(iedit-mode nil))
  )
)

;; Hideshow
(use-package hideshow
  :ensure nil
  :bind (:map hs-minor-mode-map
              ("C-`" . hs-toggle-hiding))
  :diminish hs-minor-mode
)

(provide 'core-edit)

;;----------------------------------------------------------------------------
;; core-edit.el ends here
;;----------------------------------------------------------------------------