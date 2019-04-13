;;  ========= PACKAGES INIT =========

;; The default theme
(when (require 'dracula-theme nil :noerror)
	(load-theme 'dracula t)
	)

;; Habilitar acentos ortográficos
(set-keyboard-coding-system 'utf-8)
(when (require 'iso-transl nil :noerror) )

(when (require 'whitespace nil :noerror)
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

;; Runs go fmt after save
(when (require 'go-mode nil :noerror)
	(add-hook 'before-save-hook #'gofmt-before-save)
	)

;; Runs cargo fmt after save
(when (require 'rust-mode nil :noerror)
	(setq rust-format-on-save t)
	)

;; Setup for highlight-symbol
(when (require 'highlight-symbol nil :noerror)
	(global-set-key [f3] 'highlight-symbol)
	(global-set-key [(control f3)] 'highlight-symbol-next)
	(global-set-key [(shift f3)] 'highlight-symbol-prev)
	(global-set-key [(meta f3)] 'highlight-symbol-query-replace)
	)

;; Enables flyspell
(when (require 'flyspell nil :noerror)
	;; Avoid flyspell slowdown
	(setq flyspell-issue-message-flag nil)

	;; Flyspell for latex
	(add-hook 'LaTeX-mode-hook #'turn-on-flyspell)
	)

;; Enables auto complete
;; (ac-config-default)

;; Enables powerline with default profile
(when (require 'powerline nil :noerror)
	(powerline-default-theme)
	)

(when (require 'git-gutter nil :noerror)
	(global-git-gutter-mode +1)
	)

;; Emacs Statistic mode
(when (require 'ess-site nil :noerror)

	;; Change code style
	(setq ess-default-style 'GNU)

	;; To go back to default style
	;; (setq ess-default-style 'DEFAULT)
	)

;; String Inflection for case change
(when (require 'string-inflection nil :noerror))

;;  ========= MANUALLY INSTALLED PACKAGES =========

;; Javacc-mode
(add-to-list 'load-path "~/.emacs.d/packages-extras/")
(when (require 'javacc-mode nil :noerror)
	)

;; godot-gdscript
(add-to-list 'load-path "~/.emacs.d/packages-extras/godot-gdscript.el/")
(when (require 'godot-gdscript nil :noerror)
	(add-hook 'godot-gdscript-mode-hook
						(lambda ()
							(setq-local godot-gdscript-indent-guess-indent-offset nil)
							(setq-local godot-gdscript-indent-offset 4)
							(setq-local tab-width 4)
							(setq-local indent-tabs-mode t)
							)
						)
	)
