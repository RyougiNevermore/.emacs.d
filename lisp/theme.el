;;----------------------------------------------------------------------------
;; Theme
;;----------------------------------------------------------------------------

(load-theme 'challenger-deep t)

;; let keyword, func, type font be bold
(custom-set-faces
 `(font-lock-function-name-face ((t (:bold, t))) t)
 `(font-lock-keyword-face ((t (:bold, t))) t)
 `(font-lock-builtin-face ((t (:bold, t))) t)
 `(font-lock-type-face ((t (:bold, t))) t)
 )

(provide 'theme)
