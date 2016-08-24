;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq-default indent-level 4)
(setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120))
(setq-default indent-tabs-mode nil)
(setq default-tab-width 4)
(setq-default tab-width 4)
(setq-default fill-column 100)
(setq line-number-display-limit-width 2000000)
(setq column-number-mode t)
(show-paren-mode 1)
(global-set-key "\C-xg" 'goto-line)
;; for text consoles - don't need menu bar
(unless window-system
  (menu-bar-mode 0)
)
;; Custom theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'color-to-the-max t)
;; Load paths
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; el-get
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(el-get 'sync)

;; ELPA additional repositories
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Required packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'fzf)
(require 'js2-mode)
(require 'js3-mode)
(require 'less-css-mode)
(require 'mustache-mode)
(require 'p4)
(require 'rainbow-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom set variables/faces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fzf/executable "fzfc"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Useful functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Add to hooks for modes that should indent using tabs
(defun tab-indent-setup ()
  (setq indent-tabs-mode 't))

;; Add to hooks for modes that should indent using spaces
(defun spaces-indent-setup ()
  (setq indent-tabs-mode nil))

;; Fuzzy file finding
;; Open fzf window across the bottom of the emacs session rather than the top of
;; the current window. This also makes `fzf/window-height' ineffective.
(defun fzf/start (directory)
  "Start an fzf session.
DIRECTORY indicates where to start the search."
  (let ((default-directory directory))
    (require 'term)
    (window-configuration-to-register :fzf-windows)
    (advice-add 'term-handle-exit :after #'fzf/after-term-handle-exit)
    (let ((buf (get-buffer-create "*fzf*")))
      (display-buffer "*fzf*")
      (select-window (get-buffer-window "*fzf*"))
      (if fzf/args
          (apply 'make-term "fzf" fzf/executable nil (split-string fzf/args " "))
        (make-term "fzf" fzf/executable))
      (switch-to-buffer buf)

      ;; disable various settings known to cause artifacts, see #1 for more details
      (setq-local scroll-margin 0)
      (setq-local scroll-conservatively 0)
      (setq-local term-suppress-hard-newline t) ;for paths wider than the window
      (face-remap-add-relative 'mode-line '(:box nil))

      (term-char-mode)
      (setq mode-line-format (format "   FZF  %s" directory)))))
(add-to-list 'display-buffer-alist
             `(,(rx bos "*fzf*" eos)
               (display-buffer-in-side-window)
               (side            . bottom)
               (window-height   . 0.4)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Language modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; C/C++
(setq c-default-style "stroustrup"
      c-basic-offset 4)
(add-hook 'c-mode-common-hook 'tab-indent-setup)
;; Don't indent brace that opens an in-class inline method
(c-set-offset 'inline-open 0)

;; CPerl
(add-hook 'cperl-mode-hook 'tab-indent-setup)
(setq cperl-indent-level 4
      cperl-close-paren-offset -4
      cperl-continued-statement-offset 0
      cperl-indent-parens-as-block t
      cperl-tab-always-indent t
      cperl-highlight-variables-indiscriminately t)
(add-to-list 'auto-mode-alist '("\\.\\([pP][Llm]\\|al\\)\\'" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))

(global-set-key [home] 'beginning-of-buffer)
(global-set-key [select] 'end-of-buffer)
(global-set-key [end] 'end-of-buffer)

(add-to-list 'auto-mode-alist '("\\.mi\\'" . cperl-mode))
(add-to-list 'auto-mode-alist '("\\.mc\\'" . cperl-mode))

(define-key text-mode-map (kbd "<tab>") 'tab-to-tab-stop)

(setq auto-mode-alist
      (append '(("\\.esp$" . cperl-mode)
        ("\\.pl$" . cperl-mode)
        ("\\.pm$" . cperl-mode)
        ("\\.rt$" . cperl-mode)
        ("\\.rule$" . cperl-mode)
        ("\\.sql$" . sql-mode)
        ("\\.tbl$" . sql-mode)
        ("\\.less$". css-mode)
        ("\\.css$" . css-mode))
          auto-mode-alist))

;; js3-mode
(add-hook 'js3-mode-hook 'tab-indent-setup)
(setq js3-indent-level 4
      js3-consistent-level-indent-inner-bracket t)
(add-to-list 'auto-mode-alist '("\\.json\\'" . js3-mode))

;; Haskell mode
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; Lua mode
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))
(setq lua-indent-level 4)
(add-hook 'lua-mode-hook 'spaces-indent-setup)

;; Latex mode
(add-to-list 'auto-mode-alist '("\\.tex'" . LaTeX-mode))
(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
(setq-default LaTeX-default-offset 2)
(setq-default TeX-newline-function 'newline-and-indent)

(add-to-list 'auto-mode-alist '("\\.whiskers\\'" . mustache-mode))
(setq-default mustache-basic-offset 4)
(add-hook 'mustache-mode-hook 'tab-indent-setup)

;; CSS/less settings
(add-to-list 'auto-mode-alist '("\\.less$" . less-css-mode))
(add-hook 'less-css-mode-hook 'tab-indent-setup)
(add-hook 'css-mode-hook 'tab-indent-setup)


;; Get emacs to highlight this file correctly
;; Local Variables:
;; mode: emacs-lisp
;; End:
