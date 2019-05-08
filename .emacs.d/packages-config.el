;;  ========= PACKAGES INIT =========

(eval-when-compile
	(require 'use-package))

;; The default theme
(use-package dracula-theme
	:config (load-theme 'dracula t)
	)

;; Habilitar acentos ortográficos
(use-package iso-transl
	:config (set-keyboard-coding-system 'utf-8)
	)

(use-package whitespace
	:config
	(progn
		(setq whitespace-style (quote (face spaces tabs newline space-mark tab-mark newline-mark )))
		(setq whitespace-display-mappings
					;; all numbers are unicode codepoint in decimal. e.g. (insert-char 182 1)
					'((space-mark 32 [183] [46]) ; SPACE 32 「 」, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
						(newline-mark 10 [172 10]) ; LINE FEED,
						(tab-mark 9 [124 9] [124 9]) ; tab
						)
					)
		(set-face-attribute 'whitespace-space nil :background nil :foreground "gray40")
		(set-face-attribute 'whitespace-tab nil :background nil :foreground "gray40")
		(set-face-attribute 'whitespace-newline nil :background nil :foreground "gray40")
		(global-whitespace-mode t)
		)
	)

;; Runs go fmt after save
(use-package go-mode
	:config (add-hook 'before-save-hook #'gofmt-before-save)
	)

;; Runs cargo fmt after save
(use-package rust-mode
	:config (setq rust-format-on-save t)
	)


;; Setup for highlight-symbol
(use-package highlight-symbol
	:bind
	(("<f3>" . highlight-symbol)
	 ("C-<f3>" . highlight-symbol-next)
	 ("S-<f3>" . highlight-symbol-prev)
	 ("M-<f3>" . highlight-symbol-query-replace)
	 )
	)

;; Enables flyspell
(use-package flyspell
	:config
	(progn
		;; Avoid flyspell slowdown
		(setq flyspell-issue-message-flag nil)

		;; Flyspell for latex
		(add-hook 'LaTeX-mode-hook #'turn-on-flyspell)
		)
	)

;; Enables auto complete
;; (ac-config-default)

;; Enables powerline with default profile
(use-package powerline
	:config (powerline-default-theme)
	)

(use-package git-gutter
	:config (global-git-gutter-mode +1)
	:bind
	(
	 ("C-z u" . git-gutter:update-all-windows)
	 ("C-z r" . git-gutter:revert-hunk)
	 )
	)

;; Emacs Statistic mode
(use-package ess-site
	:config
	;; Change code style
	(setq ess-default-style 'GNU)
	;; To go back to default style
	;; (setq ess-default-style 'DEFAULT)
	)

;; String Inflection for case change
(use-package string-inflection
	:bind
	(
	 ("C-z s" . string-inflection-underscore) ;; snake_scae
	 ("C-z c" . string-inflection-lowercamelcase)
	 ("C-z C" . string-inflection-camelcase)
	 )
	)

(use-package undo-tree
	:init
	(global-undo-tree-mode))

;;  ========= MANUALLY INSTALLED PACKAGES =========

;; Javacc-mode
(add-to-list 'load-path "~/.emacs.d/packages-extras/")
(use-package javacc-mode)

;; godot-gdscript
(add-to-list 'load-path "~/.emacs.d/packages-extras/godot-gdscript.el/")
(use-package godot-gdscript
	:config
	(add-hook 'godot-gdscript-mode-hook
						(lambda ()
							(setq-local godot-gdscript-indent-guess-indent-offset nil)
							(setq-local godot-gdscript-indent-offset 4)
							(setq-local tab-width 4)
							(setq-local indent-tabs-mode t)
							)
						)
	)
