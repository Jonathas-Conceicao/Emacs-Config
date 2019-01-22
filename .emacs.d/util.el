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

(global-set-key (kbd "<C-tab>") 'ibuffer) ;; Buffer Menu
(global-set-key "\C-x\C-d" "\C-a\C- \C-n\M-w\C-y") ;; Duplicate line

(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)
