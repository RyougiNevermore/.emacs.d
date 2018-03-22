;; module-markdown.el --- Desfine constants.	-*- lexical-binding: t -*-
;;
;; Author: Ryougi Nevermore <ryougi.nevermore@hotmail.com>
;; Version: 0.0.1
;; URL: https://github.com/RyougiNevermore/.emacs.d
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;             MarkDown configurations.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:



(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode))
  :config
  (when (executable-find "multimarkdown")
    (setq markdown-command "multimarkdown"))
  (setq markdown-css-paths '("http://thomasf.github.io/solarized-css/solarized-light.min.css"))

  (use-package markdown-preview-mode
    :bind (:map markdown-mode-command-map
                ("P" . markdown-preview-mode))
    :config
    (setq markdown-preview-stylesheets
          (list "https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/2.9.0/github-markdown.min.css"
                "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/styles/default.min.css" "
  <style>
   .markdown-body {
     box-sizing: border-box;
     min-width: 200px;
     max-width: 980px;
     margin: 0 auto;
     padding: 45px;
   }
   @media (max-width: 767px) {
     .markdown-body {
       padding: 15px;
     }
   }
  </style>
"))
    (setq markdown-preview-javascript
          (list "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/highlight.min.js" "
  <script>
   $(document).on('mdContentChange', function() {
     $('pre code').each(function(i, block) {
       hljs.highlightBlock(block);
     });
   });
  </script>
")))

  ;; Render and preview via `grip'
  (when (executable-find "grip")
    (defun markdown-to-html ()
      (interactive)
      (start-process "grip" "*gfm-to-html*" "grip" (buffer-file-name) "5000")
      (browse-url (format "http://localhost:5000/%s.%s"
                          (file-name-base)
                          (file-name-extension
                           (buffer-file-name)))))
    (bind-key "V" 'markdown-to-html markdown-mode-command-map)))

(provide 'module-markdown)