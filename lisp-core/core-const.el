;; core-const.el.	-*- lexical-binding: t -*-
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;             Constants.
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

(defconst sys/win32p
  (eq system-type 'windows-nt) 
  "Are we running on a WinTel system?"
)

(defconst sys/linuxp
  (eq system-type 'gnu/linux) 
  "Are we running on a GNU/Linux system?"
)

(defconst sys/macp
  (eq system-type 'darwin) 
  "Are we running on a Mac system?"
)

(defconst sys/mac-x-p
  (and (display-graphic-p) sys/macp) 
  "Are we running under X on a Mac system?"
)

(defconst sys/linux-x-p
  (and (display-graphic-p) sys/linuxp) 
  "Are we running under X on a GNU/Linux system?"
)

(defconst sys/cygwinp
  (eq system-type 'cygwin) 
  "Are we running on a Cygwin system?"
)

(defconst sys/rootp
  (string-equal "root" (getenv "USER")) 
  "Are you using ROOT user?"
)

(provide 'core-const)

;;----------------------------------------------------------------------------
;; core-const.el ends here
;;----------------------------------------------------------------------------