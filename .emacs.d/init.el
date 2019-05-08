(require 'package) 
;; Package Repositories
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Set backup directory
(setq backup-directory-alist `(("." . "~/.emacs.d/saves")))

;; ========= GENERAL EDITOR CONFIG =========

(global-display-line-numbers-mode t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(global-hl-line-mode 1)
(show-paren-mode 1)
(column-number-mode 1)
(savehist-mode 1)
;; auto insert closing bracket
(electric-pair-mode t)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(getenv "PATH")

(setq
 show-paren-style 'parenthesis
 inhibit-startup-message t
 )
(setq-default
 c-offset 2
 c-basic-offset 2
 python-indent 2
 sh-basic-offset 2
 tab-width 2
 indent-tabs-mode t

 vc-follow-symlinks t
 )

;;  ========= EXTRA CONFIG =========

;; Create prefix command to be used with C-z as prefix key
(define-prefix-command 'ctl-z-map)

(load (expand-file-name "util.el" user-emacs-directory))
(load (expand-file-name "packages-config.el" user-emacs-directory))

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)
