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

(use-package rust-mode
	:config
	(setq-default rust-format-on-save t)
	(setq-default indent-tabs-mode nil)
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
	 ("C-z n" . git-gutter:next-hunk)
	 ("C-z p" . git-gutter:previous-hunk)
	 )
	)

;; Emacs Statistic mode
(use-package ess-site
	:no-require t
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
	(global-undo-tree-mode)
	:bind
	(
	 ("M-/" . undo-tree-redo)
	 )
	)

;; Yaml mode
(use-package yaml-mode
	:config (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
	)
