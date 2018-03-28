;; mode-projectile.el	-*- lexical-binding: t -*-
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;             Projectile.
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

;; Manage and navigate projects
(use-package projectile
  :bind (("C-t" . projectile-find-file)) ; `cmd-t' or `super-t'
  :init (add-hook 'after-init-hook #'projectile-mode)
  :config
  (setq projectile-mode-line
        '(:eval (format "[%s]" (projectile-project-name)))
  )

  (setq projectile-sort-order 'recentf)
  (setq projectile-use-git-grep t)

  ;; Faster indexing on Windows
  ;; `ripgrep' is the fastest
  (when sys/win32p
    (when (executable-find "rg")
      (setq projectile-generic-command "rg -0 --files --color=never --hidden")
      (setq projectile-indexing-method 'alien)
      (setq projectile-enable-caching nil)
    )

    ;; FIXME: too slow while getting submodule files on Windows
    ;;(setq projectile-git-submodule-command ""))

    ;; Support Perforce project
    (let ((val (or (getenv "P4CONFIG") ".p4config")))
      (add-to-list 'projectile-project-root-files-bottom-up val)
    )

    ;; Rails project
    (use-package projectile-rails
      :diminish projectile-rails-mode
      :init (projectile-rails-global-mode 1)
    )
  )

  (with-eval-after-load 'evil-leader 
    (which-key-add-key-based-replacements (concat my-evil-leader-key  " p") "Project")
    (evil-leader/set-key
        "pf" 'projectile-find-file
    )
  )
)

;; Group ibuffer's list by project root
(use-package ibuffer-projectile
  :bind ("C-x C-b" . ibuffer)
  :init
  (setq ibuffer-filter-group-name-face 'font-lock-function-name-face)
  (add-hook 'ibuffer-hook
      (lambda ()
          (ibuffer-auto-mode 1)
          (ibuffer-projectile-set-filter-groups)
          (unless (eq ibuffer-sorting-mode 'alphabetic)
              (ibuffer-do-sort-by-alphabetic)
          )
      )
  )
)

(provide 'mode-projectile)

;;----------------------------------------------------------------------------
;; mode-projectile.el ends here
;;----------------------------------------------------------------------------