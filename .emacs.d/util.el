;; ========= UTILITY FUNCTIONS =========

(defun move-line (n)
	"Move the current line up or down by N lines."
	(interactive "p")
	(setq col (current-column))
	(beginning-of-line) (setq start (point))
	(end-of-line) (forward-char) (setq end (point))
	(let ((line-text (delete-and-extract-region start end)))
		(forward-line n)
		(insert line-text)
		;; restore point to original column in moved line
		(forward-line -1)
		(forward-char col)))

(defun move-line-up (n)
	"Move the current line up by N lines."
	(interactive "p")
	(move-line (if (null n) -1 (- n))))

(defun move-line-down (n)
	"Move the current line down by N lines."
	(interactive "p")
	(move-line (if (null n) 1 n)))

(defun duplicate-line ()
	"Duplicate current line and move down"
	(interactive)
	(move-beginning-of-line 1)
	(let ((beg (point)))
		(forward-line 1)
		(kill-ring-save beg (point))
		)
	(yank)
	)

(defun indent-or-complete ()
	"Complete if point is at end of a word, otherwise indent line."
	(interactive)
	(if (string-match "Minibuf" (buffer-name))
			(minibuffer-complete)
		(if mark-active
				(indent-region (region-beginning)
											 (region-end))
			(if (looking-at "\\>")
					(dabbrev-expand nil)
				(indent-for-tab-command)))))

(defun c-auto-format ()
	"Function to format C/C++ code with GNU indent"
	(interactive)
	(when (find major-mode '(c-formated-mode))
		(shell-command
		 (format "indent %s %s" (shell-quote-argument (buffer-file-name)) gnu-indent-config))
		(revert-buffer t t t)))
;; My GNU's indent config for C code
(setq gnu-indent-config "-linux -ut -ts2 -i2 -brf -bc")

(defun other-window-backward ()
  "Goto previous window"
  (interactive)
  (other-window -1)
  )

;; ========= DERIVED MODES =========

;; cmm-mode for ghc's cmm
(define-derived-mode cmm-mode c-mode 
	"cmm-mode" 
	"A variant of C mode for edditing GHC's cmm (C--) files."
	(setq indent-tabs-mode nil)
	(setq c-basic-offset 4)
	(setq tab-width 4)
	(font-lock-add-keywords 'cmm-mode ;; Function call
													'(("\\(\\(\\w\\|_\\)+\\(\\w\\|_\\|[0-9]\\)*\\)\\>\\s-*("
														 (1 font-lock-function-name-face)
														 ))
													t)
	)
(add-to-list 'auto-mode-alist '("\\.cmm\\'" . cmm-mode))

(define-derived-mode c-formated-mode c-mode
	"Formated C"
	"C that runs GNU's indent on save"
	(add-hook 'after-save-hook 'c-auto-format)
	(font-lock-add-keywords 'c-formated-mode ;; Function call
													'(("\\(\\(\\w\\|_\\)+\\(\\w\\|_\\|[0-9]\\)*\\)\\>\\s-*("
														 (1 font-lock-function-name-face)
														 ))
													t)
	)

;; ========= LANGUAGE HOOKS =========

;; Some extra hilight for C derivated modes
(add-hook
 'c-mode-hook
 (lambda()
	 (font-lock-add-keywords 'c-mode ;; Function call
													 '(("\\(\\(\\w\\|_\\)+\\(\\w\\|_\\|[0-9]\\)*\\)\\>\\s-*("
															(1 font-lock-function-name-face)
															))
													 t)
	 )
 )

;; ========= KEY BINDINGS =========

;; Remap of some keys GNU default keys
(bind-keys
 ("C-s" . isearch-forward-regexp)
 ("C-S-s" . isearch-forward)
 ("C-r" . isearch-backward-regexp)
 ("C-S-r" . isearch-backward)
 ("C-h" . delete-backward-char)
 ("M-h" . backward-kill-word)
 )

;; General key binds
(bind-keys
 ("C-<tab>" . ibuffer)

 ("M-<up>" . move-line-up)
 ("M-<down>" . move-line-down)

 ("C-x C-d" . duplicate-line)
 ("C-x p" . other-window-backward)
 )

;; My prefixed keys
(bind-keys
 :map global-map
 :prefix-map ctl-z-map
 :prefix "C-z"
 ("i" . ispell-region)
 ("C-r" . revert-buffer)
 ("C-s" . sort-lines)
 ("<tab>" . indent-or-complete)
 )
