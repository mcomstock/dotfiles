;;; emacs --- My emacs configuration file.

;;; Commentary:
;;; The .emacs file in the home directory should symlink to this file.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom set variables/faces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(async-bytecomp-package-mode 1)
 '(company-dabbrev-code-modes
   (quote
    (prog-mode batch-file-mode csharp-mode css-mode erlang-mode haskell-mode jde-mode lua-mode python-mode js2-mode js3-mode)))
 '(company-dabbrev-downcase nil)
 '(company-idle-delay 0)
 '(cperl-close-paren-offset -4)
 '(cperl-continued-statement-offset 0)
 '(cperl-font-lock t)
 '(cperl-indent-level 4)
 '(cperl-indent-parens-as-block t)
 '(cperl-invalid-face (quote trailing-whitespace))
 '(cperl-tab-always-indent t)
 '(custom-safe-themes
   (quote
    ("84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" default)))
 '(lua-indent-level 4)
 '(package-selected-packages
   (quote
    (delight flycheck-rust goto-chg toml-mode undo-tree company auto-async-byte-compile helm async flycheck mic-paren yasnippet spaceline rainbow-delimiters rust-mode haskell-mode yaml-mode rainbow-mode p4 less-css-mode json-mode js3-mode js2-mode fzf)))
 '(ruby-align-chained-calls t)
 '(ruby-align-to-stmt-keywords t)
 '(ruby-use-smie t)
 ;; whitespace
 '(whitespace-style (quote (spaces tabs newline space-mark tab-mark newline-mark))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq-default indent-level 4)
(setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120))
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default fill-column 100)
(setq-default show-trailing-whitespace t)
(setq line-number-display-limit-width 2000000)
(setq column-number-mode t)
(setq split-height-threshold 200)
(show-paren-mode 1)
(global-hl-line-mode)
(electric-indent-mode -1)
;; for text consoles - don't need menu bar
(unless window-system
  (menu-bar-mode 0)
)

;; Custom theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/color-to-the-max-theme")
(load-theme 'color-to-the-max t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ELPA additional repositories
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Required packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'async)
(require 'auto-async-byte-compile)
(require 'company)
(require 'delight)
(require 'flycheck)
(require 'goto-chg)
(require 'haskell-mode)
(require 'helm-config)
(require 'js2-mode)
(require 'js3-mode)
(require 'json-mode)
(require 'less-css-mode)
(require 'mic-paren)
(require 'powerline)
(require 'rainbow-delimiters)
(require 'rainbow-mode)
(require 'rust-mode)
(require 'spaceline-config)
(require 'toml-mode)
(require 'undo-tree)
(require 'yaml-mode)
(require 'yasnippet)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; spaceline
(defface spaceline-externally-modified-face
  '((t :background "red"))
    "Face for denoting that the buffer has been externally modified in the spaceline."
    :group 'spaceline)

(setq powerline-default-separator 'utf-8)
(setq spaceline-minor-modes-separator " ")
(setq spaceline-buffer-encoding-abbrev-p nil)
(setq spaceline-highlight-face-func 'spaceline-highlight-face-modified-externally)
(spaceline-emacs-theme)

;; async dired commands
(autoload 'dired-async-mode "dired-async.el" nil t)
(dired-async-mode 1)

;; company
(add-hook 'after-init-hook 'global-company-mode)

;; delight
(delight '((company-mode "C" "company")
           (flycheck-mode "F" "flycheck")
           (isearch-mode "I" "isearch")
           (undo-tree-mode "U" "undo-tree")
           (yas-minor-mode "Y" "yasnippet")))

;; flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; mic-paren
(paren-activate)

;; undo-tree
(global-undo-tree-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load local settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (file-readable-p "~/.emacs-local.el")
  (load "~/.emacs-local.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Useful functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Add to hooks for modes that should indent using tabs
(defun tab-indent-setup ()
  "Set the indentation style for the buffer to use tabs."
  (setq indent-tabs-mode 't))

;; Add to hooks for modes that should indent using spaces
(defun spaces-indent-setup ()
  "Set the indentation style for the buffer to use spaces."
  (setq indent-tabs-mode nil))

;; Check whether the buffer is read-only, has been modified, or has been saved since it was opened
(defun spaceline-highlight-face-modified-externally ()
  "Set the color of the spaceline based on the status of the current buffer."
  (cond
   (buffer-read-only 'spaceline-read-only)
   ((not (verify-visited-file-modtime)) 'spaceline-externally-modified-face)
   ((buffer-modified-p) 'spaceline-modified)
   (t 'spaceline-unmodified)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Jump 5 lines
(global-set-key (kbd "M-n")
                (lambda () (interactive) (forward-line 5)))
(global-set-key (kbd "M-p")
                (lambda () (interactive) (forward-line -5)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File type associatons
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))
(add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))

(setq auto-mode-alist
      (append '(("\\.esp$" . cperl-mode)
                ("\\.pl$" . cperl-mode)
                ("\\.pm$" . cperl-mode)
                ("\\.rt$" . cperl-mode)
                ("\\.rule$" . cperl-mode)
                ("\\.mi\\'" . cperl-mode)
                ("\\.mc\\'" . cperl-mode)
                ("\\.\\([pP][Llm]\\|al\\)\\'" . cperl-mode)
                ("\\.css$" . css-mode)
                ("\\.tex'" . LaTeX-mode)
                ("\\.less$". less-css-mode)
                ("\\.lua$" . lua-mode)
                ("\\.sql$" . sql-mode)
                ("\\.tbl$" . sql-mode))
              auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Language modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; C/C++
(setq c-default-style "stroustrup"
      c-basic-offset 4)
(add-hook 'c-mode-common-hook 'tab-indent-setup)
(add-hook 'c-mode-common-hook #'rainbow-delimiters-mode)
;; Don't indent brace that opens an in-class inline method
(c-set-offset 'inline-open 0)

;; CPerl
(add-hook 'cperl-mode-hook 'tab-indent-setup)
(add-hook 'cperl-mode-hook #'rainbow-delimiters-mode)

(global-set-key [home] 'beginning-of-buffer)
(global-set-key [select] 'end-of-buffer)
(global-set-key [end] 'end-of-buffer)

(define-key text-mode-map (kbd "<tab>") 'tab-to-tab-stop)

;; CSS settings
(add-hook 'css-mode-hook 'tab-indent-setup)
(add-hook 'css-mode-hook #'rainbow-delimiters-mode)

;; Emacs lisp mode
(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
;; Asyncronously compile emacs lisp files on save
(add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode)

;; Haskell mode
(add-hook 'haskell-mode-hook 'tab-indent-setup)
(add-hook 'haskell-mode-hook #'rainbow-delimiters-mode)

;; js2-mode
(add-hook 'js2-mode-hook 'tab-indent-setup)
(add-hook 'js2-mode-hook #'rainbow-delimiters-mode)

;; js3-mode
(add-hook 'js3-mode-hook 'tab-indent-setup)
(add-hook 'js3-mode-hook #'rainbow-delimiters-mode)
(setq-default js3-indent-level 4
              js3-consistent-level-indent-inner-bracket t)

;; Latex mode
(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
(add-hook 'LaTeX-mode-hook #'rainbow-delimiters-mode)
(setq-default LaTeX-default-offset 2)
(setq-default TeX-newline-function 'newline-and-indent)

;; LESS mode
(add-hook 'less-css-mode-hook 'tab-indent-setup)
(add-hook 'less-css-mode-hook #'rainbow-delimiters-mode)

;; Lua mode
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-hook 'lua-mode-hook 'spaces-indent-setup)
(add-hook 'lua-mode-hook #'rainbow-delimiters-mode)

;; Ruby mode
(add-hook 'ruby-mode-hook #'rainbow-delimiters-mode)

;; Rust mode
(add-hook 'rust-mode-hook #'rainbow-delimiters-mode)
(add-hook 'rust-mode-hook #'flycheck-rust-setup)

;; Shell mode
(add-hook 'sh-mode-hook #'rainbow-delimiters-mode)


;; Get emacs to highlight this file correctly
;; Local Variables:
;; mode: emacs-lisp
;; End:

;;; emacs ends here
