;;---------------------------------------------------------------------------
;; package elpa
;;---------------------------------------------------------------------------

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

;; cl - Common Lisp Extension
(require 'cl)

;; --------------------------------------------------------------------------
;; packages list
;; --------------------------------------------------------------------------
(defvar doaman-packages '(
			 ;; theme
			 challenger-deep-theme
                         ;; company
			 company
			  ) "Default packages")




;; --------------------------------------------------------------------------
;; Auto install packages at startup
;; --------------------------------------------------------------------------
;; set package-selectd-packages to doaman-packages
(setq package-selected-packages doaman-packages)

;; func about install packages
(defun doaman-packages-installed-p ()
  (loop for pkg in doaman-packages 
	when (not (package-installed-p pkg)) do (return nil)
	finally (return t)))

(unless (doaman-packages-installed-p)
  (message "%s" "Refreshing packages list...")
  (package-refresh-contents)
  (dolist (pkg doaman-packages) 
    (when (not (package-installed-p pkg))
      (package-install pkg))))



(provide 'packages-elpa)
