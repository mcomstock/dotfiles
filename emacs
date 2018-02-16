;;; emacs --- My emacs configuration file.

;;; Commentary:
;;; The .emacs file in the home directory should symlink to this file.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fix security flaw
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'enriched)
(defun enriched-decode-display-prop (start end &optional param)
  "Fixes security flaw http://seclists.org/oss-sec/2017/q3/422.
START start
END end
PARAM param"
  (list start end))

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
    (prog-mode batch-file-mode csharp-mode css-mode erlang-mode haskell-mode jde-mode lua-mode python-mode js2-mode)))
 '(company-dabbrev-downcase nil)
 '(company-idle-delay 0)
 '(cperl-close-paren-offset -4)
 '(cperl-continued-statement-offset 0)
 '(cperl-font-lock t)
 '(cperl-highlight-variables-indiscriminately t)
 '(cperl-indent-level 4)
 '(cperl-indent-parens-as-block t)
 '(cperl-invalid-face (quote trailing-whitespace))
 '(cperl-tab-always-indent t)
 '(custom-safe-themes
   (quote
    ("84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" default)))
 '(evil-want-C-u-scroll t)
 '(helm-swoop-speed-or-color t)
 '(linum-format "%d ")
 '(linum-relative-current-symbol "")
 '(linum-relative-format "%s ")
 '(lua-indent-level 4)
 '(package-selected-packages
   (quote
    (helm-swoop helm lua-mode use-package rjsx-mode linum-relative lsp-rust lsp-mode haxe-mode evil racer delight flycheck-rust goto-chg toml-mode undo-tree company auto-async-byte-compile async flycheck yasnippet spaceline rainbow-delimiters rust-mode haskell-mode yaml-mode rainbow-mode p4 less-css-mode json-mode fzf)))
 '(ruby-align-chained-calls t)
 '(ruby-align-to-stmt-keywords t)
 '(ruby-use-smie t)
 '(scroll-conservatively 9001)
 '(scroll-margin 5)
 '(show-paren-delay 0)
 '(whitespace-style
   (quote
    (face spaces tabs newline space-mark tab-mark newline-mark trailing))))

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
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default fill-column 100)
(setq-default show-trailing-whitespace t)

(setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120))
(setq line-number-display-limit-width 2000000)
(setq column-number-mode t)
(setq split-height-threshold 200)

(electric-indent-mode 1)
(global-auto-revert-mode t)
(global-hl-line-mode)
(show-paren-mode 1)
(xterm-mouse-mode 1)

;; Home and End keys
(global-set-key [home] 'beginning-of-buffer)
(global-set-key [select] 'end-of-buffer)
(global-set-key [end] 'end-of-buffer)

;; Tab indents to positions in tab-stop-list
(define-key text-mode-map (kbd "<tab>") 'tab-to-tab-stop)

;; for text consoles - don't need menu bar
(unless window-system
  (menu-bar-mode 0))

;; Change cursor shape depending on editing mode
(defun set-cursor-shape ()
  "Set the shape of the cursor according to the evil-mode state."
  (unless (display-graphic-p)
    (if (symbolp cursor-type)
        (send-string-to-terminal "\e[2 q"))
    (if (listp cursor-type)
        (send-string-to-terminal "\e[6 q"))))

(add-hook 'pre-command-hook 'set-cursor-shape)
(add-hook 'post-command-hook 'set-cursor-shape)

;; Make the vertical window border a solid line
(set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?│))

;; Custom theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/color-to-the-max-theme")
(load-theme 'color-to-the-max t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ELPA additional repositories
(require 'package)
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(package-initialize)

(eval-when-compile
  (require 'use-package))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Required packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package async)

(use-package auto-async-byte-compile)

(use-package company
  :delight (company-mode "C")
  :config
  (company-tng-configure-default)
  (global-company-mode))

(use-package undo-tree
  :delight (undo-tree-mode "U")
  :config
  (global-undo-tree-mode))

(use-package goto-chg)

(use-package evil
  :after (undo-tree goto-chg)
  :config
  (evil-mode 1)
  ;; Use space like leader key
  (define-key evil-motion-state-map " " nil)

  ;; General commands
  (define-key evil-motion-state-map " ac" 'company-mode)
  (define-key evil-motion-state-map " fc" 'flycheck-mode)
  (define-key evil-motion-state-map " ln" 'linum-mode)
  (define-key evil-motion-state-map " hf" 'helm-find-files)
  (define-key evil-motion-state-map (kbd "SPC RET") 'helm-M-x)
  (define-key evil-motion-state-map " hm" 'helm-multi-swoop-all)
  (define-key evil-motion-state-map " hs" 'helm-swoop)
  (define-key evil-motion-state-map " wf" 'which-function-mode)
  (define-key evil-motion-state-map " tw" 'whitespace-mode)
  (define-key key-translation-map " x" (kbd "C-x")))


(use-package flycheck
  :delight (flycheck-mode "F")
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules))

(use-package haskell-mode
  :commands haskell-mode
  :config
  (add-hook 'haskell-mode-hook #'rainbow-delimiters-mode))

(use-package haxe-mode
  :commands haxe-mode)

(use-package helm-config)

(use-package helm
  :after (helm-config)
  :delight (helm-mode "H")
  :config
  (helm-mode 1)
  (global-set-key (kbd "C-x C-f") 'helm-find-files))

(use-package helm-swoop
  :after (helm))

(use-package json-mode
  :commands json-mode)

(use-package less-css-mode
  :commands less-css-mode
  :config
  (add-hook 'less-css-mode-hook #'rainbow-delimiters-mode))

(use-package linum-relative
  :config
  (linum-relative-on))

(use-package lsp-flycheck
  :after (flycheck lsp-mode))

(use-package lsp-mode)

(use-package lsp-rust
  :after (lsp-mode lsp-flycheck))

(use-package lua-mode
  :commands lua-mode
  :config
  (add-hook 'lua-mode-hook #'rainbow-delimiters-mode))

(use-package powerline)

(use-package racer
  :after (rust)
  :commands racer-mode
  :delight (racer-mode "R"))

(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode)

(use-package rainbow-mode
  :commands rainbow-mode)

(use-package rjsx-mode
  :commands rjsx-mode
  :config
  (add-hook 'js2-mode-hook #'rainbow-delimiters-mode))

(use-package rust-mode
  :commands rust-mode
  :config
  (add-hook 'rust-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'rust-mode-hook #'flycheck-rust-setup)
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode))

(use-package spaceline-config
  :config
  (setq powerline-default-separator 'utf-8)
  (setq spaceline-minor-modes-separator " ")
  (setq spaceline-buffer-encoding-abbrev-p nil)
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  (spaceline-emacs-theme))

(use-package toml-mode
  :commands toml-mode)

(use-package yaml-mode
  :commands yaml-mode)

(use-package yasnippet
  :commands yas-minor-mode
  :delight (yas-minor-mode "Y"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; async dired commands
(autoload 'dired-async-mode "dired-async.el" nil t)
(dired-async-mode 1)

;; delight
(delight '((eldoc-mode "E" "eldoc")
           (isearch-mode "I" "isearch")
           (racer-mode "R" "racer")
           (whitespace-mode "W" "whitespace")))

;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
(defun my/use-eslint-from-node-modules ()
  "Use local eslint from node_modules before global."
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

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
                ("\\.js$" . rjsx-mode)
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
(add-hook 'c-mode-common-hook #'rainbow-delimiters-mode)
;; Don't indent brace that opens an in-class inline method
(c-set-offset 'inline-open 0)

;; CPerl
(add-hook 'cperl-mode-hook #'rainbow-delimiters-mode)
(add-hook 'cperl-mode-hook
          (lambda ()
            (setq-local company-backends
                        '(company-dabbrev-code company-keywords company-oddmuse company-dabbrev))))

;; CSS settings
(add-hook 'css-mode-hook #'rainbow-delimiters-mode)

;; Emacs lisp mode
(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
;; Asyncronously compile emacs lisp files on save
(add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode)

;; Latex mode
(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
(add-hook 'LaTeX-mode-hook #'rainbow-delimiters-mode)
(setq-default LaTeX-default-offset 2)
(setq-default TeX-newline-function 'newline-and-indent)

;; Ruby mode
(add-hook 'ruby-mode-hook #'rainbow-delimiters-mode)

;; Shell mode
(add-hook 'sh-mode-hook #'rainbow-delimiters-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load local settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (file-readable-p "~/.emacs-local.el")
  (load "~/.emacs-local.el"))


;; Get emacs to highlight this file correctly
;; Local Variables:
;; mode: emacs-lisp
;; End:

;;; emacs ends here
