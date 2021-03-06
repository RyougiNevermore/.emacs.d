;; mode-ivy.el	-*- lexical-binding: t -*-
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;             Ivy.
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

  
(use-package counsel
    :diminish ivy-mode counsel-mode
    :bind (
        ("C-s" . swiper)
        ("C-S-s" . swiper-all)
        ("C-c C-r" . ivy-resume)
        ("C-c v" . ivy-push-view)
        ("C-c V" . ivy-pop-view)
        ("M-x" . counsel-M-x)
        :map counsel-mode-map
        ([remap swiper] . counsel-grep-or-swiper)
        ("C-x C-f" . counsel-find-file)
        ("C-x C-r" . counsel-recentf)
        ("C-x v" . counsel-describe-variable)
        ("C-x f" . counsel-describe-function)
        ("C-x j" . counsel-mark-ring)
        ("C-c C-p" . counsel-package)

        ("C-c c L" . counsel-find-library)
        ("C-c c a" . counsel-apropos)
        ("C-c c e" . counsel-colors-emacs)
        ("C-c c f" . counsel-fzf)
        ("C-c c g" . counsel-grep)
        ("C-c c h" . counsel-command-history)
        ("C-c c i" . counsel-git)
        ("C-c c j" . counsel-git-grep)
        ("C-c c l" . counsel-load-library)
        ("C-c c m" . counsel-minibuffer-history)
        ("C-c c o" . counsel-outline)
        ("C-c c p" . counsel-pt)
        ("C-c c r" . counsel-rg)
        ("C-c c s" . counsel-ag)
        ("C-c c u" . counsel-unicode-char)
        ("C-c c w" . counsel-colors-web)

        :map ivy-minibuffer-map
        ("s-c" . ivy-yank-word)

        :map counsel-find-file-map
        ("C-h" . counsel-up-directory)

        :map swiper-map
        ("M-%" . swiper-query-replace))
    :init
    (add-hook 'after-init-hook #'ivy-mode)
    (add-hook 'ivy-mode-hook #'counsel-mode)
    :config
    (setq enable-recursive-minibuffers t) ; Allow commands in minibuffers

    (setq ivy-use-selectable-prompt t)
    (setq ivy-use-virtual-buffers t)    ; Enable bookmarks and recentf
    (setq ivy-height 10)
    (setq ivy-count-format "(%d/%d) ")
    (setq ivy-on-del-error-function nil)
    ;; (setq ivy-initial-inputs-alist nil)

    (setq ivy-re-builders-alist 
        '( (read-file-name-internal . ivy--regex-fuzzy)
            (t . ivy--regex-plus)
          )
    )

    (setq swiper-action-recenter t)
    (setq counsel-find-file-at-point t)
    (setq counsel-yank-pop-separator "\n-------\n")

    ;; Find counsel commands quickly
    (bind-key "<f6>" (lambda ()
                        (interactive)
                        (counsel-M-x "^counsel ")
                     )
    )

    ;; Use faster search tools: ripgrep or the silver search
    (let ((command
                (cond
                    ((executable-find "rg") "rg -i -M 120 --no-heading --line-number --color never '%s' %s")
                    ((executable-find "ag") "ag -i --noheading --nocolor --nofilename --numbers '%s' %s")
                )
        ))
        (setq counsel-grep-base-command command)
    )

    ;; Integration with `projectile'
    (with-eval-after-load 'projectile
        (setq projectile-completion-system 'ivy)
    )

    ;; Integration with `magit'
    (with-eval-after-load 'magit
        (setq magit-completing-read-function 'ivy-completing-read)
    )

    ;; Additional key bindings for Ivy
    (use-package ivy-hydra
        :bind (:map ivy-minibuffer-map
                ("M-o" . ivy-dispatching-done-hydra))
    )

    ;; Correcting words with flyspell via Ivy
    (use-package flyspell-correct-ivy
        :after flyspell
        :bind (:map flyspell-mode-map
                ("C-;" . flyspell-correct-previous-word-generic))
    )

    ;; More friendly display transformer for Ivy
    (use-package ivy-rich
        :init
        (setq ivy-virtual-abbreviate 'full
            ivy-rich-switch-buffer-align-virtual-buffer t)
        (setq ivy-rich-path-style 'abbrev)

        (ivy-set-display-transformer 'ivy-switch-buffer
                                 'ivy-rich-switch-buffer-transformer)

        (with-eval-after-load 'counsel-projectile
            (ivy-set-display-transformer 'counsel-projectile
                                   'ivy-rich-switch-buffer-transformer)
            (ivy-set-display-transformer 'counsel-projectile-switch-to-buffer
                                   'ivy-rich-switch-buffer-transformer)
        )
    )

    ;; Ivy integration for Projectile
    (use-package counsel-projectile
        :init (counsel-projectile-mode 1)
    )

    ;; Stylesheet-selector-aware swiper
    (use-package counsel-css
        :bind (:map counsel-mode-map
                    ("C-c c c" . counsel-css))
        :init (add-hook 'css-mode-hook 'counsel-css-imenu-setup)
    )

    ;; Display world clock using Ivy
    (use-package counsel-world-clock
        :bind (:map counsel-mode-map
                    ("C-c c k" . counsel-world-clock))
    )

    ;; Tramp ivy interface
    (use-package counsel-tramp
        :bind (:map counsel-mode-map
                    ("C-c c t" . counsel-tramp))
    )

    ;; Ivy for GNU global
    (use-package counsel-gtags
        :diminish counsel-gtags-mode
        :bind (:map counsel-gtags-mode-map
                ("M-." . counsel-gtags-find-definition)
                ("M-r" . counsel-gtags-find-reference)
                ("M-s" . counsel-gtags-find-symbol)
                ("M-," . counsel-gtags-go-backward))
        :init
        (setq counsel-gtags-auto-update t)

        (add-hook 'c-mode-hook 'counsel-gtags-mode)
        (add-hook 'c++-mode-hook 'counsel-gtags-mode)
    )
 
)



(provide 'mode-ivy)

;;----------------------------------------------------------------------------
;; mode-ivy.el ends here
;;----------------------------------------------------------------------------