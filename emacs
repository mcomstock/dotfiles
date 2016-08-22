;; General settings
(setq-default indent-level 4)
(setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120))
(setq-default indent-tabs-mode nil)
(setq default-tab-width 4)
(setq-default tab-width 4)
(setq-default fill-column 100)
(setq line-number-display-limit-width 2000000)

;; CUSTOM SET VARIABLES
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("5088486fc726284544ced63caddc5017ccf7dec34dca96e23b4bb80fd4aa90fc" "f2f9d14c75173569496370f7a67c594920a217e167f579190c0d3b23fa459576" "8ca26cc4adf2f0b701065b7e20f624661b7ea28972b1d38013b8b05b6458348a" "767653b4af523c0879b00790603df63f274dbef8e36ef38bf332b270db851fd3" "12f96adc775b66c1151f907d4bf2e78fe6076e69921b0171bef5c9dd645646b0" "de2824c729a7bb54041b27a771f27fb53fbacf0f2a5d6ed4a7bf6c407a8775e8" "8d7dd17331334b8870c7feaa18c7dd8cd4f485272ae1d0c330e76b58143bde72" "3b75c0b6e4a476bc60ac14a808a39be0faa5d5bece4caabf3098bcde96c018ac" "58191e713cf0e5fe2461f58080634430203364a73b4b2ec95017d446c3c989cf" "1ea8f698a1000d52af60f2802efa3d82aa98592f644a9b52a0612b52e12f9074" "eff6d22dbbfaa091a14fce3c5feb1643cf92d99f57c1653c0efb7d89901a3c56" "311ea667335f713bcd02c48f02839a53c4045cd16b19f7dc7e65a57deb1b3219" "8c4588777c30eeb6d7fe4f014d8ef70f4e1a95aeb2b762838a5b78fbdd918259" "c2df7a17234f8b8a537d014898eea72abf191446b2f71589c305eecfb33c5d29" "04c3e6d857fbb19d47687f1e6397d983c8890c7e4d35dbc031b2f993d753f822" "37629a9e0c5d154e3b4887c97eb0c97b34e7a09fc9ff3def89573f5b018a69a5" "91d59a25e78598de55fbf7f6f986794d95b4fc0588f13fb32af78c9f25e20340" "adc76146e8c6e05d8b103b3fcf7baf72081348550c78ac80b6c350e58b98a17a" "033cd89c59b8376685c6016e7561bd14928399d5146aee9c619f70ee9fcc1cd4" "5bdd7f9fa272f82a11e6b36a41e50d6e8fe0474025d4194ece5d3a208abea808" "6abaff62141237ae528408b78b59e33db830f7b760fa40b10585107c23549e1a" "234834080fe2f44db9c0e501f602d952605bf110a81a3b43043695528b9d9a33" "d175100b67fceb07a465e2ca9bd964577f3d32acaf6ac5c6a5f905d9410d87b0" "1285b912cad9d055127b0fc91981c1d371a063d6ab5781474d2353df57e19b3f" "2a961838bb9e95f41322ca7346386da1ba9566588d557447252f9b7e508698e9" "7dacedc34995eb017f1d8d53749d74360d0fa79776237c134b0544748777e7b0" "e477bc184f0b908f6bf7c80f5c98bebf635f2e8c6116e926ab3d4b54ce479cd2" "49ead36a21358f306d71dda0eececb7317194c31a186dd590bd7a78774192aed" "4d40eaf49434ce9bfbb77098d3d73615b7b26a4ce5e9482c47acb5abbe063d73" "08d7501d1e397ec17ebb24313e770844740311799d4c9170edac62651f955d78" "bd54f27ad5f1a3bb16cc69f4abc3d45a5e6fa301b527dd89bb0c6d84b0970615" "4d35aeffc72e875c7b431f3816f36d23777a7d4cb4ef5059f81ef69638e51d2c" "45ac07a21fdbe86b66fbc9e8db135092070345ba327e6b925d1c6eb0ed7176bf" "b50f1acf7d0f897e747c2a9a9ad64fea7583d188aff00d886d2f1ee07c681553" "b96228067334fe9c621d796fc4fff6259510a179ee29e375d04b245ea1c991f9" "a6ba5289a4bd8a09e3c157099d5e996c47178b763848762153723092d5a7f062" "c45cafdbf7885dd38c913afe62dcfaa6aabc6b1e95ece8073b273ca7bed0237e" "654b637acee49c0ec2da3578a2ca600413f16ff691929b93cd8a18d9c9df31de" "9d00b35f152e04f538e82e1ba38f055e6fc5c7073bd703d4f61972ff3641f9c5" "536a52b24fbfacebb27ac40aa3c9add7e9acfab1d19a9fd74fde296194e3a15d" "e42db8e280fec237d7568f0e2e8cdca6c89a2cf8e5961a316b1debf104a63709" "d8ac4c34b38d430127099f7d6f51ea32514d3434eecdd93afc55cc4dfb151b9c" "883f8b3450646a43ca9b099f4b7b9eeba8c3fefdd51e5319d54b8b4ffdea4ed2" "f66a379f09547b168d682e5d1333a3379c0806f34fdc8a823992bd5eac7cfc62" "feec065917da868f36ec3425ca249a5f9aa0566df51faab29243923e98698c80" "60fe0677985dc637453fdc30e4e3aa44cce3e61c408c07218e40d14ed0e8eb30" "5de584e940145d2a269395af93c9695332737013d198d00d7e89cf6a75dc7913" "cbc887caa9fe3e25708859531feb6aa1e04c97ac63a8142e571cd9ad4b330f10" "6b4b1356f32663d7fc45dd79e26971301d64a2188e2bca2b66cc024a00967db4" "6d838ea266c1895a918501b7fe767652f0195712b764032e75a557d8b7d63948" "f932d76e91fef8f995e75f1caa4459f4c53293e2df28602ac2f269441a6fcf27" "afb4e3defbb3fd714ae86f917762599c4c3183f5265e637bd1e57a3269418ec0" "56a6123d3c07277470280dac96ba7e9bf07386f1bfeba229c3318da8cc11e132" "0d754626f07e517d24dcde034d7c808a0605b6d4579e9808e96abc46b3136635" "75a00a1d3a3fc87cb3e3a3f556409dcb8308a8f5426f5365fead7094fcc68423" "01d2f4579ab8cc15f6d35e0840634d7da2fe7b92396edab4d9fc8768becc3574" "08e454e293bf4105220684997cd17b42b8a93ef860d1b217c8ed2b496789a205" "d3cc091b339af21cfee9c8ad49dde020cf131dc6fa364c040688b1a70632b8cf" default)))
 '(fzf/executable "fzfc"))

;; CUSTOM SET FACES
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Add to hooks for modes that should indent using tabs
(defun tab-indent-setup ()
  (setq indent-tabs-mode 't))

;; C/C++ Indentation
(setq c-default-style "stroustrup"
      c-basic-offset 4)

(add-hook 'c-mode-common-hook 'tab-indent-setup)

;; Don't indent brace that opens an in-class inline method
(c-set-offset 'inline-open 0)

;; for text consoles - don't need menu bar
(unless window-system
  (menu-bar-mode 0)
)

;; cperl settings
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


(setq column-number-mode t)
(show-paren-mode 1)

(global-set-key [home] 'beginning-of-buffer)
(global-set-key [select] 'end-of-buffer)
(global-set-key [end] 'end-of-buffer)

(add-to-list 'auto-mode-alist '("\\.mi\\'" . cperl-mode))
(add-to-list 'auto-mode-alist '("\\.mc\\'" . cperl-mode))

(global-set-key "\C-xg" 'goto-line)
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

;; el-get stuff
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

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

;; Lua settings
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

(setq lua-indent-level 4)

(defun lua-indent-setup ()
  (setq indent-tabs-mode nil))

(add-hook 'lua-mode-hook 'lua-indent-setup)

;; Custom theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'color-to-the-max t)

;; Latex stuff
(add-to-list 'auto-mode-alist '("\\.tex'" . LaTeX-mode))
(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
(setq-default LaTeX-default-offset 2)
(setq-default TeX-newline-function 'newline-and-indent)

;; P4 stuff
(add-to-list 'load-path "~/.emacs.d/p4.el")
(require 'p4)

;; Mustache (whiskers)
(add-to-list 'load-path "~/.emacs.d/moustache")
(require 'mustache-mode)
(add-to-list 'auto-mode-alist '("\\.whiskers\\'" . mustache-mode))
(setq-default mustache-basic-offset 4)
(add-hook 'mustache-mode-hook 'tab-indent-setup)

;; CSS/less settings
(add-to-list 'load-path "~/.emacs.d/less-css-mode")
(require 'less-css-mode)
(add-hook 'less-css-mode-hook 'tab-indent-setup)
(add-hook 'css-mode-hook 'tab-indent-setup)

;; Fuzzy file finding
;; Open fzf window across the bottom of the emacs session rather than the top of
;; the current window. This also makes `fzf/window-height' ineffective.
(require 'fzf)
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

;; A cool defadvice macro for advising several functions the same way
(defmacro advise-commands (advice-name commands location &rest body)
  "Apply advice named ADVICE-NAME to multiple COMMANDS.
The advice type (before around after) is in LOCATION.
The body of the advice is in BODY."
  `(progn
     ,@(mapcar (lambda (command)
                 `(defadvice ,command (,location ,(intern (concat (symbol-name command) "-" advice-name)) activate)
                    ,@body))
               commands)))

;; Make ediff commands save my window config and reload it after it's done
(defvar pre-p4-ediff-window-state nil)
(advise-commands
 "save-window-configuration"
 (ediff-buffers ediff-buffers3 ediff-files ediff-files3 ediff-directories ediff-directories3)
 before
 (setq pre-p4-ediff-window-state (current-window-configuration)))
(defadvice ediff-quit (after ediff-quit-after activate)
  "Reset the window configuration after `ediff-quit'."
  (set-window-configuration pre-p4-ediff-window-state))

