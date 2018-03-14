;;--------------------------------------------------------------------------
;; key binding of edit
;;--------------------------------------------------------------------------

(when (not *is-a-mac*)
  ;; select all 
  (global-set-key (kbd "s-a") 'mark-whole-buffer)
  ;; save buffer
  (global-set-key (kbd "s-s") 'save-buffer)
  ;; copy
  (global-set-key (kbd "s-c") 'kill-ring-save)
  ;; paste
  (global-set-key (kbd "s-v") 'yank)
  ;; cut
  (global-set-key (kbd "s-x") 'kill-region)
  ;; undo
  (global-set-key (kbd "s-z") 'undo)
  )

(provide 'system-kbd)
