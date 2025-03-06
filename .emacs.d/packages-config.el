;;  ========= PACKAGES INIT =========

(eval-when-compile
	(require 'use-package))

(use-package quelpa
	:config (setq quelpa-self-upgrade-p nil))

(require 'quelpa-use-package)

;; The default theme
(use-package dracula-theme
	:config (load-theme 'dracula t)
	)

;; Habilitar acentos ortográficos
(use-package iso-transl
	:config (set-keyboard-coding-system 'utf-8)
	)

(use-package whitespace
	:diminish global-whitespace-mode
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

(use-package yasnippet
	:diminish yas-minor-mode
	:config
	(progn

		;; Inspired by: https://emacs.stackexchange.com/questions/26271/
		(defvar-local yas--expandable-keys-overlay nil)
		(defun yas-show-expand-keys ()
			;; Put overlay on text which is an expandable snippet key.
			(let ((keys-at-point (and yas-minor-mode (yas--templates-for-key-at-point)))
						(have-overlay (overlayp (buffer-local-value 'yas--expandable-keys-overlay (current-buffer)))))
				(if keys-at-point
						(let ((beg (nth 1 keys-at-point))
									(end (nth 2 keys-at-point)))
							(if have-overlay
									(move-overlay yas--expandable-keys-overlay beg end)
								(setq-local yas--expandable-keys-overlay
														(make-overlay beg end)))
							(overlay-put yas--expandable-keys-overlay 'face '(:box t)))
					(when have-overlay
						(delete-overlay yas--expandable-keys-overlay)))))
		(add-hook 'post-command-hook #'yas-show-expand-keys)


		(yas-global-mode t)
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
	:diminish git-gutter-mode
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
	:diminish undo-tree-mode
	:init
	(global-undo-tree-mode)
	:bind
	(
	 ("M-/" . undo-tree-redo)
	 )
	:config
	;; Save swap-files (*.~undo-tree~) on dotemacs dir
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
	)



;; Yaml mode
(use-package yaml-mode
	:config (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
	)

(use-package org
  :config
  (progn
    ;; Allow evaulation of the following source blocks in Org-Mode
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (haskell . t)
       (shell . t)
       (R . t)
       (python . t)
       (C . t)
       (java . t)
       )
     )

    (setq org-startup-folded 'content)
    ;; Run any code block without confirmation
    (setq org-confirm-babel-evaluate 'nil)
    )
  )

;; Add support for :async keyword for org source blocks
(use-package ob-async)

(use-package projectile
  :diminish projectile-mode
  :bind-keymap ("C-c C-p" . projectile-command-map)
  :hook (after-init . projectile-mode)
  :config
  (add-hook 'find-file-hook
            (lambda ()
              (when (file-remote-p default-directory)
                (setq-local projectile-mode-line "Projectile")
                )
              )
            )
  ;; (add-to-list 'projectile-globally-ignored-directories "build_*")
  )

(use-package magit
  :config
  (require 'git-commit)
  (add-hook 'git-commit-mode-hook 'flyspell-mode)
  (add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell)
  (add-hook 'git-commit-mode-hook (lambda () (setq fill-column 72)))
  (setq magit-diff-refine-hunk t)
  :bind ((("C-z g" . magit-file-dispatch))))

(use-package ansi-color
	:config
	(progn

		;; Inspired by: https://stackoverflow.com/a/23382008
		(defun display-ansi-colors ()
			(interactive)
			(let ((inhibit-read-only t))
				(ansi-color-apply-on-region (point-min) (point-max))))
		)
	)
