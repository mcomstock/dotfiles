;;; emacs --- My emacs configuration file.

;;; Commentary:
;;; The .emacs file in the home directory should symlink to this file.

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
 '(company-minimum-prefix-length 1)
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
 '(elm-indent-offset 2)
 '(evil-want-C-u-scroll t)
 '(helm-swoop-speed-or-color t)
 '(linum-format "%d ")
 '(linum-relative-current-symbol "")
 '(linum-relative-format "%s ")
 '(lua-indent-level 4)
 '(package-selected-packages
   (quote
    (helm-config eglot elm-mode evil-org vue-mode projectile-rails yard-mode gitignore-mode coffee-mode helm-ag helm-projectile projectile haml-mode evil-search-highlight-persist evil-nerd-commenter evil-args macrostep evil-anzu winum which-key evil-surround helm-swoop helm lua-mode use-package rjsx-mode linum-relative haxe-mode evil delight goto-chg toml-mode undo-tree company auto-async-byte-compile async flycheck yasnippet rainbow-delimiters rust-mode haskell-mode yaml-mode rainbow-mode p4 less-css-mode json-mode fzf)))
 '(ruby-align-chained-calls t)
 '(ruby-align-to-stmt-keywords t)
 '(ruby-insert-encoding-magic-comment nil)
 '(ruby-use-smie t t)
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
(load-theme 'outrun t)

;; Function to allow right-alignment on the mode line
;; Found here: https://stackoverflow.com/questions/16775855/how-to-fixate-value-on-the-right-side-of-the-modeline
(defun mode-line-fill (reserve)
    "Return empty space leaving RESERVE space on the right."
    (unless reserve
      (setq reserve 20))
    (when (and window-system (eq 'right (get-scroll-bar-mode)))
      (setq reserve (- reserve 3)))
    (propertize " "
                'display `((space :align-to (- (+ right right-fringe right-margin) ,reserve)))))

;; Mode line
(setq-default mode-line-format
              '("%e"
                ;; mode-line-front-space
                " "
                (:eval (winum-get-number-string))
                "  "
                mode-line-mule-info
                ;; mode-line-client
                mode-line-modified
                ;; mode-line-remote
                ;; mode-line-frame-identification
                "  "
                mode-line-buffer-identification
                "  "
                (vc-mode vc-mode)
                "  "
                (flycheck-mode flycheck-mode-line)
                "  "
                mode-line-modes
                "  "
                mode-line-misc-info
                ;; Add enough space to right-align everything that follows
                (:eval (mode-line-fill
                        (+
                         (seq-reduce '+
                                     (seq-map 'length mode-line-position)
                                     0)
                         (length evil-mode-line-tag))))
                mode-line-position
                evil-mode-line-tag))

;; Use which-function mode all the time
(add-hook 'prog-mode-hook #'which-function-mode)

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

(use-package async
  :ensure t)

(use-package auto-async-byte-compile
  :ensure t)

(use-package coffee-mode
  :ensure t
  :commands coffee-mode)

(use-package company
  :delight (company-mode " C")
  :ensure t
  :config
  (company-tng-configure-default)
  (global-company-mode))

(use-package undo-tree
  :delight
  :ensure t
  :config
  (global-undo-tree-mode))

(use-package goto-chg
  :ensure t)

(use-package eglot
  :ensure t
  :commands (eglot eglot-ensure))

(use-package elm-mode
  :commands elm-mode
  :ensure t
  :config
  (add-hook 'elm-mode-hook
            (lambda () (setq-local indent-level 2)
              (setq-local evil-shift-width 2))))

(use-package evil
  :ensure t
  :after (undo-tree goto-chg)
  :config
  (evil-mode 1)
  ;; Use space like leader key
  (define-key evil-motion-state-map " " nil)

  (require 'evil-anzu)

  ;; General commands
  (define-key evil-motion-state-map " ac" 'company-mode)
  (define-key evil-motion-state-map " fc" 'flycheck-mode)
  (define-key evil-motion-state-map " ln" 'linum-mode)
  (define-key evil-motion-state-map " hf" 'helm-find-files)
  (define-key evil-motion-state-map (kbd "SPC RET") 'helm-M-x)
  (define-key evil-motion-state-map " hm" 'helm-multi-swoop-all)
  (define-key evil-motion-state-map " hs" 'helm-swoop-without-pre-input)
  (define-key evil-motion-state-map " nm" 'normal-mode)
  (define-key evil-motion-state-map " wf" 'which-function-mode)
  (define-key evil-motion-state-map " tw" 'whitespace-mode)
  (define-key key-translation-map " x" (kbd "C-x")))

(use-package evil-args
  :ensure t
  :after (evil)
  :config
  (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
  (define-key evil-outer-text-objects-map "a" 'evil-outer-arg))

(use-package evil-nerd-commenter
  :ensure t
  :after (evil)
  :config
  (define-key evil-motion-state-map " ;" 'evilnc-comment-or-uncomment-lines))

(use-package evil-org
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package evil-search-highlight-persist
  :ensure t
  :after (evil)
  :config
  (global-evil-search-highlight-persist t)
  (define-key evil-motion-state-map "  " 'evil-search-highlight-persist-remove-all))

(use-package evil-surround
  :ensure t
  :after (evil)
  :config
  (global-evil-surround-mode 1))


(use-package flycheck
  :ensure t
  :delight
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules))

(use-package haml-mode
  :ensure t
  :defer t)

(use-package haskell-mode
  :ensure t
  :commands haskell-mode
  :config
  (add-hook 'haskell-mode-hook #'rainbow-delimiters-mode))

(use-package haxe-mode
  :ensure t
  :commands haxe-mode)

(use-package helm
  :ensure t
  :after (helm-config)
  :delight
  :config
  (helm-mode 1)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-x b") 'helm-mini))

(use-package helm-ag
  :ensure t
  :after (helm)
  :config
  (setq helm-ag-base-command "rg --vimgrep --no-heading")

  (define-key evil-motion-state-map " hg" 'helm-ag))

(use-package helm-config
  ;; Not acutally its own package
  :ensure f)

(use-package helm-projectile
  :ensure t
  :after (helm helm-ag projectile)
  :config
  (setq projectile-completion-system 'helm)

  ;; Workaround necessary for using ripgrep instead of ag
  (defun helm-projectile-ag (&optional options)
    "Helm version of projectile-ag."
    (interactive (if current-prefix-arg (list (read-string "option: " "" 'helm-ag--extra-options-history))))
    (if (require 'helm-ag nil  'noerror)
        (if (projectile-project-p)
            (let ((helm-ag-command-option options)
                  (current-prefix-arg nil))
              (helm-do-ag (projectile-project-root) (car (projectile-parse-dirconfig-file))))
          (error "You're not in a project"))
      (error "Sorry, helm-ag not available")))

  (define-key evil-motion-state-map " pf" 'helm-projectile)
  (define-key evil-motion-state-map " pg" 'helm-projectile-ag))

(use-package helm-swoop
  :ensure t
  :after (helm))

(use-package json-mode
  :ensure t
  :commands json-mode)

(use-package less-css-mode
  :ensure t
  :commands less-css-mode
  :config
  (add-hook 'less-css-mode-hook #'rainbow-delimiters-mode))

(use-package linum-relative
  :ensure t
  :config
  (linum-relative-on))

(use-package lua-mode
  :ensure t
  :commands lua-mode
  :config
  (add-hook 'lua-mode-hook #'rainbow-delimiters-mode))

(use-package macrostep
  :ensure t
  :commands (macrostep-mode macrostep-expand))

(use-package projectile
  :ensure t
  :delight '(:eval (concat " " (projectile-project-name)))
  :config
  (define-key evil-motion-state-map " pm" 'projectile-mode))

(use-package projectile-rails
  :ensure t
  :after (projectile evil)
  :commands projectile-rails-mode
  :config
  (define-key key-translation-map " pr" (kbd "C-c r")))

(use-package rainbow-delimiters
  :ensure t
  :commands rainbow-delimiters-mode)

(use-package rainbow-mode
  :ensure t
  :commands rainbow-mode)

(use-package rjsx-mode
  :ensure t
  :commands rjsx-mode
  :config
  (add-hook 'js2-mode-hook #'rainbow-delimiters-mode))

(use-package rust-mode
  :after (eglot)
  :ensure t
  :commands rust-mode
  :init
  (add-hook 'rust-mode-hook #'rainbow-delimiters-mode)
  :config
  (eglot-ensure))

(use-package toml-mode
  :ensure t
  :commands toml-mode)

(use-package which-key
  :ensure t
  :config
  (setq which-key-idle-delay 0.5)
  (which-key-mode)
  :delight)

(use-package winum
  :ensure t
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
  :ensure t
  :commands yaml-mode)

(use-package yard-mode
  :ensure t
  :commands yard-mode
  :delight (yard-mode " YD"))

(use-package yasnippet
  :ensure t
  :commands yas-minor-mode
  :delight (yas-minor-mode " Y"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; async dired commands
(autoload 'dired-async-mode "dired-async.el" nil t)
(dired-async-mode 1)

;; delight
(delight '((eldoc-mode " E" "eldoc")
           (isearch-mode " I" "isearch")
           (whitespace-mode " W" "whitespace")))

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
(add-hook 'ruby-mode-hook #'yard-mode)
(add-hook 'ruby-mode-hook #'projectile-rails-mode)
(add-hook 'ruby-mode-hook
          (lambda ()
            (setq-local evil-shift-width ruby-indent-level)))

;; Shell mode
(add-hook 'sh-mode-hook #'rainbow-delimiters-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load local settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (file-readable-p "~/.emacs-local.el")
  (load "~/.emacs-local.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Revert startup performance optimizations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Reset garbage collection settings to the default as late as possible.
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 800000)
            (setq gc-cons-percentage 0.1)))

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist config--file-name-handler-alist)))


;; Get emacs to highlight this file correctly
;; Local Variables:
;; mode: emacs-lisp
;; End:

;;; emacs ends here
