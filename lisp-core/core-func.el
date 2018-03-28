;; core-func.el.	-*- lexical-binding: t -*-
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;             Functions.
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

;; Dos2Unix/Unix2Dos
(defun dos2unix ()
  "Convert the current buffer to UNIX file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil)
)

(defun unix2dos ()
  "Convert the current buffer to DOS file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos nil)
)

;; Revert buffer
(defun revert-current-buffer ()
  "Revert the current buffer."
  (interactive)
  (message "Revert this buffer.")
  (revert-buffer t t)
)

(bind-key "<f5>" 'revert-current-buffer)
(bind-key "s-r" 'revert-current-buffer)

;; Update configurations
(defun update-config ()
  "Update Emacs configurations to the latest version."
  (interactive)
  (message "Updating Emacs configurations...")
  (cd "~/.emacs.d/")
  (shell-command "git pull")
  (message "Update finished. Restart Emacs to complete the process.")
  
)


(provide 'core-func)


;;----------------------------------------------------------------------------
;; core-func.el ends here
;;----------------------------------------------------------------------------