(require 'package) 
;; Package Repositories
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Set backup directory
(setq backup-directory-alist `(("." . "~/.emacs.d/saves")))

;; Display line on buffers
(global-linum-mode t)

;; Disable bars in GUI
(menu-bar-mode -1)
(tool-bar-mode -1)

;; Tab width
(setq-default tab-width 2)

;; turn on highlighting current line
(global-hl-line-mode 1)

;; turn on bracket match highlight
(show-paren-mode 1)

;; highlight brackets
(setq show-paren-style 'parenthesis)

;; show cursor position within line
(column-number-mode 1)

;; save minibuffer history
(savehist-mode 1)

;; auto insert closing bracket
;; (electric-pair-mode 1)

;; Export env variable to EMACS
(getenv "PATH")

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

;; Fix colors on Shell
(eval-after-load 'shell
   '(progn
      (autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
      (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on t)
      t))
(setq ansi-color-names-vector
  ["black" "red" "green" "yellow" "PaleBlue" "magenta" "cyan" "white"])

;; My personal Key Bindings
(global-set-key (kbd "<C-tab>") 'buffer-menu) ;; Buffer Menu
(global-set-key "\C-x\C-d" "\C-a\C- \C-n\M-w\C-y") ;; Duplicate line

(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)

;; Habilitar acentos ortográficos
(set-keyboard-coding-system 'utf-8)
(require 'iso-transl)

(require 'whitespace)
;; Config for whitespace-mode
;; Make whitespace-mode with very basic background coloring for whitespaces.
;; Edited from: http://ergoemacs.org/emacs/whitespace-mode.html
(setq whitespace-style (quote (face spaces tabs newline space-mark tab-mark newline-mark )))
;; Make whitespace-mode and whitespace-newline-mode use “¬” for end of line char and “»” for tab.
(setq whitespace-display-mappings
;; all numbers are unicode codepoint in decimal. e.g. (insert-char 182 1)
      '((space-mark 32 [183] [46]) ; SPACE 32 「 」, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
	(newline-mark 10 [172 10]) ; LINE FEED,
	(tab-mark 9 [124 9] [124 9]) ; tab
	))
(set-face-attribute 'whitespace-space nil :background nil :foreground "gray40")
(set-face-attribute 'whitespace-tab nil :background nil :foreground "gray40")
(set-face-attribute 'whitespace-newline nil :background nil :foreground "gray40")
(global-whitespace-mode 1)

;; Load Atom theme as default
(load-theme 'atom-one-dark t)

;; Golang configs
;; (require 'go-autocomplete)
;; (require 'auto-complete-config)
;; (add-to-list 'load-path "~/.emacs.d/golang/")

;; Runs go fmt after save
(add-hook 'before-save-hook #'gofmt-before-save)

;; Setup for highlight-symbol
(require 'highlight-symbol)
(global-set-key [f3] 'highlight-symbol)
(global-set-key [(control f3)] 'highlight-symbol-next)
(global-set-key [(shift f3)] 'highlight-symbol-prev)
(global-set-key [(meta f3)] 'highlight-symbol-query-replace)

;; Enables flyspell
(require 'flyspell)

;; Avoid flyspell slowdown
(setq flyspell-issue-message-flag nil)

;; Flyspell for latex
(add-hook 'LaTeX-mode-hook #'turn-on-flyspell)

;; Enables auto complete
;; (ac-config-default)

;; Enables powerline with default profile
(require 'powerline)
(powerline-default-theme)

(require 'git-gutter)
(global-git-gutter-mode +1)

(setq wakatime-api-key "38377444-781c-4473-994e-1a3fca37ac3c")
(global-wakatime-mode)

