;; init.el --- My emacs configuration file. -*- lexical-binding: t; -*-

;;; Commentary:
;;; Loads packages and settings for my Emacs configuration.  To use, create a symlink to this file
;;; at ~/.emacs.d/init.el.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Startup performance optimizations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Avoid garbage collection during startup for speed. Must be undone at the end of the file.
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

;; Turn off file-name-handler-alist during startup. Must be undone at the end of the file.
(defvar config--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fix security flaw
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (version< emacs-version "25.3")
  (require 'enriched)
  (defun enriched-decode-display-prop (start end &optional param)
    "Fixes security flaw http://seclists.org/oss-sec/2017/q3/422.
START start
END end
PARAM param"
    (list start end)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom set variables/faces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(async-bytecomp-allowed-packages (quote (all)))
 '(async-bytecomp-package-mode 1)
 '(c-basic-offset 4)
 '(c-default-style "stroustrup")
 '(company-dabbrev-code-modes
   (quote
    (prog-mode batch-file-mode csharp-mode css-mode erlang-mode haskell-mode jde-mode lua-mode python-mode js2-mode)))
 '(company-dabbrev-downcase nil)
 '(company-idle-delay 0)
 '(company-lsp-cache-candidates (quote auto))
 '(company-minimum-prefix-length 1)
 '(cperl-close-paren-offset -4)
 '(cperl-continued-statement-offset 0)
 '(cperl-font-lock t)
 '(cperl-highlight-variables-indiscriminately t)
 '(cperl-indent-level 4)
 '(cperl-indent-parens-as-block t)
 '(cperl-invalid-face (quote trailing-whitespace))
 '(cperl-tab-always-indent t)
 '(css-indent-offset 2)
 '(custom-safe-themes
   (quote
    ("84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" default)))
 '(elm-indent-offset 2)
 '(evil-want-C-u-scroll t)
 '(js-indent-level 2)
 '(js-switch-indent-offset 2)
 '(lua-indent-level 4)
 '(package-selected-packages
   (quote
    (dockerfile-mode typescript-mode flycheck-inline lsp-ui company-lsp lsp-mode diff-hl counsel-projectile counsel swiper ivy markdown-mode elixir-mode racer flycheck-rust eglot elm-mode evil-org vue-mode projectile-rails yard-mode gitignore-mode coffee-mode projectile haml-mode evil-nerd-commenter evil-args macrostep evil-anzu winum which-key evil-surround lua-mode use-package js2-mode rjsx-mode haxe-mode evil delight goto-chg toml-mode undo-tree company auto-async-byte-compile async flycheck yasnippet rainbow-delimiters rust-mode haskell-mode yaml-mode rainbow-mode less-css-mode json-mode)))
 '(ruby-align-chained-calls t)
 '(ruby-align-to-stmt-keywords t)
 '(ruby-insert-encoding-magic-comment nil)
 '(ruby-use-smie t t)
 '(scroll-conservatively 9001)
 '(scroll-margin 5)
 '(show-paren-delay 0)
 '(typescript-indent-level 2)
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

;; Source: https://emacs.stackexchange.com/a/37270/2418
(defun simple-mode-line-render (left right)
  "Return a string of `window-width' length containing LEFT, and RIGHT aligned respectively."
  (let* ((available-width (- (window-total-width) (+ (length (format-mode-line left)) (length (format-mode-line right))))))
    (append left (list (format (format "%%%ds" available-width) "")) right)))

;; Set the mode line format
(setq-default
 mode-line-format
 '((:eval
    (simple-mode-line-render
     ;; left
     '("%e "
       mode-line-mule-info
       mode-line-modified
       " " mode-line-buffer-identification
       " " (vc-mode vc-mode)
       " " mode-line-modes
       ;; (flycheck-mode flycheck-mode-line)
       " " mode-line-misc-info)
     ;; right
     '("%p %l:%c ")))))

(defun display-line-numbers-relative-toggle ()
  "Toggle display of relative line numbers."
  (interactive)
  (if (eq display-line-numbers 'relative)
      (setq-local display-line-numbers nil)
    (setq-local display-line-numbers 'relative)))

(defun init--set-indent-level-2 ()
  "Locally set the indent level to 2 spaces."
  (setq-local indent-level 2)
  (setq-local evil-shift-width 2))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Prevent package initialization, which is quite slow.
(setq package-enable-at-startup nil)

;; ELPA additional repositories
(require 'package)
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))

;; Rather than calling (package-initialize), manually set the load path.
(let ((default-directory "~/.emacs.d/elpa/"))
  (normal-top-level-add-subdirs-to-load-path))

(require 'use-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Required packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package anzu
  :defer 1)

(use-package async
  :defer 1
  :config
  (autoload 'dired-async-mode "dired-async.el" nil t)
  (dired-async-mode 1))

(use-package auto-async-byte-compile)

(use-package cc-mode
  :mode
  ("\\.\\(cc\\|hh\\)\\'" . c++-mode)
  ("\\.[ch]\\(pp\\|xx\\|\\+\\+\\)\\'" . c++-mode)
  ("\\.\\(CC?\\|HH?\\)\\'" . c++-mode)
  ("\\.c\\'" . c-mode)
  ("\\.h\\'" . c-or-c++-mode)
  ("\\.y\\(acc\\)?\\'" . c-mode)
  ("\\.lex\\'" . c-mode)
  ("\\.i\\'" . c-mode)
  ("\\.ii\\'" . c++-mode)
  ("\\.java\\'" . java-mode)
  ("\\.m\\'" . objc-mode)
  ("\\.idl\\'" . idl-mode)
  ("\\.\\(u?lpc\\|pike\\|pmod\\(\\.in\\)?\\)\\'" . pike-mode)
  ("awk" . awk-mode)
  ("mawk" . awk-mode)
  ("nawk" . awk-mode)
  ("gawk" . awk-mode)
  :interpreter
  ("pike" . pike-mode)
  ("\\.awk\\'" . awk-mode)
  :config
  (add-hook 'c-mode-common-hook #'rainbow-delimiters-mode)

  ;; Don't indent brace that opens an in-class inline method
  (c-set-offset 'inline-open 0))

(use-package css-mode
  :mode
  ("\\.css$" . css-mode)
  ("\\.scss\\'" . scss-mode)
  :config
  (add-hook 'css-mode-hook #'rainbow-delimiters-mode))

(use-package coffee-mode
  :mode
  ("\\.coffee\\'" . coffee-mode)
  ("\\.iced\\'" . coffee-mode)
  ("Cakefile\\'" . coffee-mode)
  ("\\.cson\\'" . coffee-mode)
  :interpreter
  ("coffee" . coffee-mode)
  :commands coffee-mode)

(use-package company
  :delight
  :defer 1
  :config
  (company-tng-configure-default)
  (global-company-mode))

(use-package company-lsp
  :after (company lsp-mode)
  :init
  (push 'company-lsp company-backends))

(use-package company-tng
  :commands (company-tng-configure-default))

(use-package counsel
  :commands (counsel-M-x counsel-find-file)
  :delight
  :config
  (counsel-mode 1))

(use-package counsel-projectile
  :commands (counsel-projectile counsel-projectile-rg))

(use-package cperl-mode
  :mode
  ("\\.esp$" . cperl-mode)
  ("\\.pl$" . cperl-mode)
  ("\\.pm$" . cperl-mode)
  ("\\.rt$" . cperl-mode)
  ("\\.rule$" . cperl-mode)
  ("\\.mi\\'" . cperl-mode)
  ("\\.mc\\'" . cperl-mode)
  ("\\.\\([pP][Llm]\\|al\\)\\'" . cperl-mode)
  :interpreter
  ("miniperl" . cperl-mode)
  ("perl" . cperl-mode)
  ("perl5" . cperl-mode)
  :config
  (add-hook 'cperl-mode-hook #'rainbow-delimiters-mode)
  (defun init--set-perl-company-backends ()
    "Set company backends for perl."
    (setq-local company-backends
                '(company-dabbrev-code company-keywords company-oddmuse company-dabbrev)))
  (add-hook 'cperl-mode-hook 'init--set-perl-company-backends))

(use-package delight
  :commands (delight))

(use-package diff-hl
  :commands (global-diff-hl-mode diff-hl-mode)
  :config
  (require 'diff-hl-margin)
  (require 'diff-hl-flydiff)
  (diff-hl-margin-mode)
  (diff-hl-flydiff-mode))

(use-package dockerfile-mode
  :commands (dockerfile-mode)
  :mode
  ("\\Dockerfile\\'" . dockerfile-mode))

(use-package undo-tree
  :delight
  :config
  (global-undo-tree-mode))

(use-package goto-chg)

(use-package eglot
  :commands (eglot eglot-shutdown))

(use-package eldoc
  :commands (eldoc-mode)
  :delight)

(use-package elisp-mode
  :commands (elisp-mode)
  :mode
  ("\\.elc\\'" . elisp-byte-code-mode)
  :config
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode))

(use-package elixir-mode
  :mode
  ("\\.elixir\\'" . elixir-mode)
  ("\\.ex\\'" . elixir-mode)
  ("\\.exs\\'" . elixir-mode))

(use-package elm-mode
  :mode
  ("\\.elm\\'" . elm-mode)
  :config
  (add-hook 'elm-mode-hook 'init--set-indent-level-2))

(use-package evil
  :after (undo-tree goto-chg)
  :commands (evil-mode)
  :init
  (defun init--enable-evil-mode ()
    "Turn on evil-mode."
    (evil-mode 1))
  (add-hook 'after-init-hook 'init--enable-evil-mode)
  :config
  (evil-select-search-module 'evil-search-module 'evil-search)
  ;; Use space like leader key
  (define-key evil-motion-state-map " " nil)

  ;; For some reason these ones need to be specified manually
  (autoload 'swiper "swiper.el" nil t)
  (autoload 'swiper-all "swiper.el" nil t)

  ;; General commands
  (define-key evil-motion-state-map "  " 'evil-ex-nohighlight)
  (define-key evil-motion-state-map " u" 'universal-argument)
  (define-key evil-motion-state-map " ac" 'company-mode)
  (define-key evil-motion-state-map " fc" 'flycheck-mode)
  (define-key evil-motion-state-map " fm" 'flymake-mode)
  (define-key evil-motion-state-map " fi" 'flycheck-inline-mode)
  (define-key evil-motion-state-map " ln" 'display-line-numbers-mode)
  (define-key evil-motion-state-map " lr" 'display-line-numbers-relative-toggle)
  (define-key evil-motion-state-map " sm" 'lsp)
  (define-key evil-motion-state-map " ss" 'lsp-shutdown-workspace)
  (define-key evil-motion-state-map " sr" 'lsp-restart-workspace)
  (define-key evil-motion-state-map " hf" 'counsel-find-file)
  (define-key evil-motion-state-map (kbd "SPC RET") 'counsel-M-x)
  (define-key evil-motion-state-map " hm" 'swiper-all)
  (define-key evil-motion-state-map " hs" 'swiper)
  (define-key evil-motion-state-map " nm" 'normal-mode)
  (define-key evil-motion-state-map " wf" 'which-function-mode)
  (define-key evil-motion-state-map " tw" 'whitespace-mode)
  (define-key evil-motion-state-map " dh" 'global-diff-hl-mode)
  (define-key evil-motion-state-map " em" 'eglot)
  (define-key evil-motion-state-map " es" 'eglot-shutdown)
  (define-key evil-motion-state-map " pm" 'projectile-mode)
  (define-key evil-motion-state-map " pf" 'counsel-projectile)
  (define-key evil-motion-state-map " pg" 'counsel-projectile-rg)
  (define-key evil-motion-state-map " ;" 'evilnc-comment-or-uncomment-lines)

  (define-key key-translation-map " x" (kbd "C-x")))

(use-package evil-anzu
  :after (evil anzu))

(use-package evil-args
  :after (evil)
  :commands (evil-inner-arg evil-outer-arg)
  :init
  (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
  (define-key evil-outer-text-objects-map "a" 'evil-outer-arg))

(use-package evil-nerd-commenter
  :after (evil)
  :commands (evilnc-comment-or-uncomment-lines))

(use-package evil-org
  :delight
  :after (evil org)
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook 'evil-org-set-key-theme)
  (evil-org-agenda-set-keys))

(use-package evil-org-agenda
  :commands (evil-org-agenda-set-keys))

(use-package evil-surround
  :after (evil)
  :config
  (global-evil-surround-mode 1))


(use-package flycheck
  :commands (flycheck-mode global-flycheck-mode)
  :init
  (add-hook 'prog-mode-hook #'flycheck-mode)
  ;; :delight
  :config
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

  (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules))

(use-package flycheck-inline
  :commands (flycheck-inline-mode))

(use-package flycheck-rust
  :after (flycheck)
  :delight
  :commands (flycheck-rust-setup))

(use-package flymake
  :commands (flymake-mode)
  :config
  (defun init--show-flymake-message ()
    "Show any flymake errors under the cursor in the minibuffer"
    (when (and flymake-mode
               (get-char-property (point) 'flymake-diagnostic))
      (message (flymake--diag-text (get-char-property (point) 'flymake-diagnostic)))))

  (add-hook 'post-command-hook #'init--show-flymake-message))

(use-package gitignore-mode
  :commands (gitignore-mode))

(use-package haml-mode
  :mode
  ("\\.haml\\'" . haml-mode))

(use-package haskell-mode
  :mode
  ("\\.[gh]s\\'" . haskell-mode)
  ("\\.hsig\\'" . haskell-mode)
  ("\\.l[gh]s\\'" . literate-haskell-mode)
  ("\\.hsc\\'" . haskell-mode)
  :interpreter
  ("runghc" . haskell-mode)
  ("runhaskell" . haskell-mode)
  :config
  (add-hook 'haskell-mode-hook #'rainbow-delimiters-mode))

(use-package haxe-mode
  :mode
  ("\\.hx\\'" . haxe-mode))

(use-package isearch
  :delight (isearch-mode " I"))

(use-package ivy
  :delight
  :commands (counsel-M-x
             counsel-descbinds
             counsel-describe-function
             counsel-describe-variable
             counsel-apropos
             counsel-describe-face
             counsel-faces
             counsel-find-file
             counsel-imenu
             counsel-load-library
             counsel-load-theme
             counsel-yank-pop
             counsel-info-lookup-symbol
             counsel-mark-ring
             counsel-bookmark)
  :init
  (setq ivy-use-selectable-prompt t)
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-use-virtual-buffers t)
  :config
  (ivy-mode 1)

  (setq ivy-extra-directories nil)
  (define-key ivy-minibuffer-map (kbd "RET") 'ivy-alt-done))

(use-package js
  :commands (js-mode)
  :delight (js-mode "JS")
  :init
  (add-hook 'js-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'js-mode-hook 'init--set-indent-level-2))

(use-package js2-mode
  :mode
  ("\\.js$" . js2-mode)
  ("\\.jsx\\'" . js2-mode))

(use-package json-mode
  :mode
  ("\\.json\\'" . json-mode))

(use-package less-css-mode
  :mode
  ("\\.less\\'" . less-css-mode)
  :config
  (add-hook 'less-css-mode-hook #'rainbow-delimiters-mode))

(use-package lua-mode
  :mode
  ("\\.lua\\'" . lua-mode)
  :interpreter
  ("lua" . lua-mode)
  :init
  (dolist (name (list "node" "nodejs" "gjs" "rhino"))
    (add-to-list 'interpreter-mode-alist (cons (purecopy name) 'js-mode)))

  (add-hook 'lua-mode-hook #'rainbow-delimiters-mode))

(use-package lsp-mode
  :commands (lsp)
  :init
  (setq-default lsp-prefer-flymake nil))

(use-package lsp-ui
  :commands (lsp-ui-mode)
  :init
  (setq-default lsp-ui-flycheck-enable t))

(use-package macrostep
  :commands (macrostep-mode macrostep-expand))

(use-package markdown-mode
  :mode
  ("\\.markdown\\'" . markdown-mode)
  ("\\.md\\'" . markdown-mode))

(use-package mouse
  :if (and (eq system-type 'darwin)
           (not window-system))
  :config
  (xterm-mouse-mode t)

  (defun init--scroll-down-1 ()
    "Scroll down 1."
    (interactive)
    (scroll-down 1))
  (defun init--scroll-up-1 ()
    "Scroll up 1."
    (interactive)
    (scroll-up 1))

  (global-set-key [mouse-4] 'init--scroll-down-1)
  (global-set-key [mouse-5] 'init--scroll-up-1)

  (defun track-mouse (e))
  (setq mouse-sel-mode t))

(use-package org
  :commands (org-mode))

(use-package vice-theme
  :init
  (setq custom-theme-directory "~/.emacs.d/themes")
  (load-theme 'vice t))

(use-package projectile
  :delight '(:eval (concat " " (projectile-project-name)))
  :after (evil)
  :commands (projectile-mode))

(use-package projectile-rails
  :commands projectile-rails-mode
  :delight
  :config
  (define-key key-translation-map " pr" (kbd "C-c r")))

(use-package racer
  :delight (racer-mode " R")
  :commands (racer-mode))

(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode)

(use-package rainbow-mode
  :commands rainbow-mode)

(use-package recentf
  :defer 1)

(use-package rjsx-mode
  :commands rjsx-mode
  :init
  (add-hook 'js2-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'js2-mode-hook #'init--set-indent-level-2))

(use-package ruby-mode
  :commands ruby-mode
  :mode
  ("\\.rb\\'" . ruby-mode)
  :interpreter
  ("ruby" . ruby-mode)
  :init
  (add-to-list 'auto-mode-alist
               (cons (purecopy (concat "\\(?:\\.\\(?:"
                                       "rbw?\\|ru\\|rake\\|thor"
                                       "\\|jbuilder\\|rabl\\|gemspec\\|podspec"
                                       "\\)"
                                       "\\|/"
                                       "\\(?:Gem\\|Rake\\|Cap\\|Thor"
                                       "\\|Puppet\\|Berks"
                                       "\\|Vagrant\\|Guard\\|Pod\\)file"
                                       "\\)\\'"))
                     'ruby-mode))

  (dolist (name (list "ruby" "rbx" "jruby" "ruby1.9" "ruby1.8"))
    (add-to-list 'interpreter-mode-alist (cons (purecopy name) 'ruby-mode)))

  (add-hook 'ruby-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'ruby-mode-hook #'yard-mode)
  (add-hook 'ruby-mode-hook #'projectile-rails-mode)
  (defun init--set-ruby-shift-width ()
    "Set the evil-shift-width for ruby-mode."
    (setq-local evil-shift-width ruby-indent-level))
  (add-hook 'ruby-mode-hook 'init--set-ruby-shift-width))

(use-package rust-mode
  :mode
  ("\\.rs\\'" . rust-mode)
  :init
  (add-hook 'rust-mode-hook #'rainbow-delimiters-mode))

(use-package sh-script
  :commands (sh-mode)
  :config
  (add-hook 'sh-mode-hook #'rainbow-delimiters-mode))

(use-package swiper
  :after (ivy)
  :commands (swiper swiper-all swiper-avy))

(use-package tex-mode
  :mode
  ("\\.tex'" . latex-mode)
  :config
  (add-hook 'latex-mode-hook 'turn-on-auto-fill)
  (add-hook 'latex-mode-hook #'rainbow-delimiters-mode)
  (setq-default LaTeX-default-offset 2)
  (setq-default TeX-newline-function 'newline-and-indent))

(use-package toml-mode
  :commands toml-mode)

(use-package typescript-mode
  :mode
  ("\\.ts$" . typescript-mode)
  :init
  (add-hook 'typescript-mode-hook #'rainbow-delimiters-mode))

(use-package vue-mode
  :mode
  ("\\.vue\\'" . vue-mode))

(use-package which-key
  :defer 1
  :delight
  :config
  (setq which-key-idle-delay 0.5)
  (which-key-mode))

(use-package whitespace
  :commands (whitespace-mode)
  :delight (whitespace-mode " W"))

(use-package winum
  :defer 1
  :after (evil)
  :config
  (setq winum-auto-setup-mode-line nil)
  (winum-mode)
  (define-key evil-motion-state-map " 1" 'winum-select-window-1)
  (define-key evil-motion-state-map " 2" 'winum-select-window-2)
  (define-key evil-motion-state-map " 3" 'winum-select-window-3)
  (define-key evil-motion-state-map " 4" 'winum-select-window-4)
  (define-key evil-motion-state-map " 5" 'winum-select-window-5)
  (define-key evil-motion-state-map " 6" 'winum-select-window-6)
  (define-key evil-motion-state-map " 7" 'winum-select-window-7)
  (define-key evil-motion-state-map " 8" 'winum-select-window-8)
  (define-key evil-motion-state-map " 9" 'winum-select-window-9))

(use-package yaml-mode
  :mode
  ("\\.\\(e?ya?\\|ra\\)ml\\'" . yaml-mode)
  :config
  (add-hook 'yaml-mode-hook #'flycheck-mode))

(use-package yard-mode
  :commands yard-mode
  :delight (yard-mode))

(use-package yasnippet
  :defer 1
  :commands yas-minor-mode
  :delight (yas-minor-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Revert startup performance optimizations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Reset garbage collection settings to the default as late as possible.
(defun init--reset-gc ()
  "Reset GC behavior."
  (setq gc-cons-threshold 800000)
  (setq gc-cons-percentage 0.1))
(add-hook 'emacs-startup-hook 'init--reset-gc)

(defun init--reset-file-name-handler ()
  "Reset the file name handler."
  (setq file-name-handler-alist config--file-name-handler-alist))
(add-hook 'emacs-startup-hook 'init--reset-file-name-handler)

(add-hook 'emacs-startup-hook 'package-initialize)

;; Local Variables:
;; no-byte-compile: t
;; End:

(provide 'init)
;;; init.el ends here
