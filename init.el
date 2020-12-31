;;; emacs-lisp; coding
;;; -*- coding:utf-8 -*-
;;; Common Lisp extensions for Emacs
(require 'cl)

;;; set default mode to scrach buffer
(setq default-major-mode 'emacs-lisp-mode)

;;; debug on
;; (setq debug-on-error t)

;;; server start for emacs client
(require 'server)
(unless (server-running-p)
  (server-start))

;;; use global env path
(add-to-list 'load-path "~/.emacs.d/lisp")

;;; call coding utf8 (C-c u)
(require 'coding-utf8)
;;; call 1row-scroll
(require '1row-scroll)
;;; call reloader
(require 'reloader)
;;; https://github.com/danamlund/emacs-makefile-runner
(require 'makefile-runner)
(global-set-key (kbd "<C-f11>") 'makefile-runner)

;;; Set language
(set-language-environment "Japanese")
;;; ?? which one is better utf-8 or utf-8-unix ??
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

;;; Set Background and Forground color
(setq default-frame-alist
	  (append
	   (list
		'(background-color . "gray20")
		'(foreground-color . "gray85")
		'(cursor-color . "yellow"))
	  default-frame-alist))

;;; Paren
(show-paren-mode 1)

;;; global key setting
;;; For key of C-h(backspace)
;; (global-set-key "\C-h" 'backward-delete-char)
;;; For key of newline
(global-set-key "\C-m" 'newline-and-indent)

;;; key of backspace
(normal-erase-is-backspace-mode 0)

;;; indent
(setq-default indent-width 2)

;;; default indent 
(setq-default c-basic-offset 2
	tab-width 2
	indent-tabs-mode nil)

;;; no make backup file
(setq backup-inhibited t)
(setq make-backup-files nil)
(setq auto-save-default nil)

;;; delete auto-save-files when exit emacs
(setq delete-auto-save-files t)

;;; bar setting
;;(tool-bar-mode 0)
;;(menu-bar-mode 0)
(scroll-bar-mode 0)

;;; No init-screen
(setq inhibit-splash-screen t)

;;; No newline
(setq next-line-add-newlines nil)

;;; C-mode
(add-hook
	'c-mode-common-hook
	(lambda ()
	;;; bsd style
	(c-set-style "bsd")
	;;; tab indent, no space
	(setq indent-tabs-mode t)
	;;; tab -> 2
	(setq c-basic-offset 2)
	(setq tab-width 2)
	;;; input ';' , goto next column
  ;;; and delete brank (too bad)
	;; (c-toggle-auto-hungry-state 1)
  ;;; delete brank
    (C-toggle-hungry-state 1)
	))

;;; Display-fill-column-indicator-mode
;; Sets 80 columns rule.
(setq-default fill-column 80)
;; Supports display-fill-column-indicator-mode at since ver27.
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

;;; Time
(setq display-time-day-and-date t)
(display-time)
(setq display-time-string-forms
	'((format "%s/%s/%s(%s) %s:%s"
		year month day
		dayname
		24-hours minutes)))

;;; Indicate Function
(which-function-mode 1)

;;; insert template
(auto-insert-mode t)
;;; auto complete in mini buffer
(icomplete-mode 1)

;;; Indicate Cursor
(column-number-mode t)
(line-number-mode t)

;;; No beep
(setq visible-bell t)

;;; Yatex mode
(setq auto-mode-alist (cons '("\\.tex$" . yatex-mode) auto-mode-alist))
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
;(setq load-path
;	(append '(expand-file-name
;		"/usr/local/share/emacs/site-lisp/yatex") load-path))
;;; Set character code
;;; 0: non converion
;;; 1: Shift JIS
;;; 2: ISO-2022-JP
;;; 3: EUC
;;; 4: UTF-8
(setq YaTeX-kanji-code 4)

;;; Yahtml mode
(autoload 'yahtml-mode "yahtml" "Yet Another HTML mode" t)
(setq auto-mode-alist (cons (cons "\\.html?$" 'yahtml-mode) auto-mode-alist))
(setq yahtml-lint-program "htmllint")
(setq yahtml-kanji-code 4)  
(add-hook 'yahtml-mode-hook '(lambda () (setq fill-column 80)))

;;; Emacs-mozc
;;(require 'mozc)
;;(set-language-environment "Japanese")
;;(setq default-input-method "japanese-mozc")
;;(setq candidates-style 'echo-area)
;;(global-set-key (kbd "C-o") 'toggle-input-method)
;;(setq mozc-candidate-style 'overlay)

;;; load site-lisp/misc
;;(setq load-path
;;	(append '(expand-file-name
;;		"/usr/local/share/emacs/site-lisp/misc/") load-path))

;;; Windows.el
;;; default is "C-c C-w"
;;; If windows.el is set,
;;; you don't need set (require 'revive).
;;; This already has loaded by including windows.el
(require 'windows)
;; no make new frame
(setq win:use-frame nil)
(win:startup-with-window)
;; C-x C-c saves this state
;; C-x C don't saves.
(define-key ctl-x-map "\C-c" 'see-you-again)
(define-key ctl-x-map "C" 'save-buffers-kill-emacs)

;;; unformal repositories 'mepla' and 'marmalade'
;;; --- emacs package install ---
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)
(setq package-enable-at-startup nil)
;;; --- emacs package install ---
(defvar my-packages '(
                      ;;; --- useful tools ---
                      use-package
                      which-key
                      smex
                      ido-completing-read+
                      ido-vertical-mode
                      ido-select-window
                      ido-migemo
                      diminish
                      symbol-overlay
                      volatile-highlights
                      highlight-indent-guides
                      flycheck
                      ;;; --- refactoring ---
                      anzu
                      ;;; --- auto-complete ---
                      auto-complete
                      company
                      company-quickhelp
                      company-quickhelp-terminal
                      ;;; --- load path from shell ---
                      exec-path-from-shell
                      ;;; --- ruby mode ---
                      ruby-mode
                      ruby-electric
                      ruby-block
                      ruby-refactor
                      ;;; --- python mode ---
                      python-mode
                      elpy
                      jedi
                      py-autopep8
                      flymake-easy
                      flymake-python-pyflakes
                      pyvenv
                      ;;; --- go mode ---
                      go-mode
                      go-dlv
                      ;;; --- vue mode ---
                      vue-mode
                      mmm-mode
                      vue-html-mode
                      ssass-mode
                      edit-indirect
                      ;;; --- typescript mode ---
                      typescript-mode
                      tide
                      ;;; --- scss mode ---
                      scss-mode
                      ;;; --- web mode ---
                      web-mode
                      ;;; --- coffee sript ---
                      coffee-mode
                      coffee-fof
                      ;;; --- php mode ---
                      php-mode
                      php-completion
                      ;;; --- clojure mode ---
                      clojure-mode
                      paredit
                      cider
                      rainbow-delimiters
                      ac-nrepl
                      ;;; --- ocaml util ---
                      caml
                      tuareg
                      ;flymake-tuareg
                      ;; --- scala util ---
                      scala-mode
                      sbt-mode
                      ;;; --- lsp(language server protocol) mode ---
                      lsp-mode
                      lsp-ui
                      company-lsp
                      ;;; --- markdown mode ---
                      markdown-mode
                      ;;; --- org mode ---
                      org
                      ;;; --- yaml-mode ---
                      yaml-mode
                      ;;; --- dockerfile-mode ---
                      dockerfile-mode
                      ;;; --- cvs-mode ---
                      csv-mode
                      ;;; --- plantuml-mode ---
                      plantuml-mode
                      ))
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-refresh-contents)
    (package-install p)))

;;; --- which-key
;; reference: https://qiita.com/Ladicle/items/feb5f9dce9adf89652cf
(use-package which-key
  :diminish which-key-mode
  :hook (after-init . which-key-mode))

;;; --- ido-mode (Emacs default)
;; reference: https://qiita.com/tadsan/items/33ebb8db2271897a462b
(ido-mode 1)
(ido-everywhere 1)
(setq ido-confirm-unique-completion t)
(setq ido-enable-prefix t)
(setq ido-enable-dot-prefix t)
(setq ido-enable-flex-matching t)
(setq ido-auto-merge-delay-time 5)

;; Use smex(ido; Emacs default completion mode), Not use amx.
(use-package smex
  :ensure t
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)
         ("C-c C-c M-x" . execute-extended-command))
  :config
  (smex-initialize))

;; reference: https://www-he.scphys.kyoto-u.ac.jp/member/shotakaha/dokuwiki/doku.php?id=toolbox:emacs:ido:start
;;
;; If it helps, I get the warning Warning (bytecomp): 
;;   reference to free variable ‘ido-cr+-minibuffer-depth’ when starting up Emacs, 
;;   but this only started when just now switching from ido-ubiquitous to ido-completing-read+.
;;   I made the package change upon noticing the message about the name change to switch to ido-completing-read+.
;;   Before that, never had such messages.
;;
;; reference: https://github.com/DarwinAwardWinner/ido-completing-read-plus/issues/121
;(require 'ido-ubiquitous)
(require 'ido-completing-read+)
(ido-ubiquitous-mode 1)

(use-package ido-vertical-mode
  :config
  (ido-vertical-mode 1)
  ;; C-n and C-p selects candidated words
  (setq ido-vertical-define-keys 'C-n-and-C-p-only)
  (setq ido-vertical-show-count t))
;;
;; --- Cloudn't search hidden files, too bad!
;; (use-package ido-migemo
;;   :ensure t
;;   :config
;;   (setq ido-migemo-exclude-command-list '(smex swith-to-buffer ido-switch-buffer persp-switch ido-describe-bindings))
;;   (setq ido-migemo-prompt-string "=[ido-migemo]=")
;;   (ido-mode 1)
;;   (ido-migemo-mode 1))
;;
;; ---I think it's not useful for me because I use windows.el
;;(use-package ido-select-window
;;  :ensure t
;;  :bind ("C-x o" . ido-select-window))

;; reference: https://www.yokoweb.net/2017/03/05/emacs-ubuntu-migemo/
(when (and (executable-find "cmigemo")
           (require 'migemo nil t))
  (setq migemo-command "cmigemo")
  (setq migemo-options '("-q" "--emacs"))

  (setq migemo-dictionary "/usr/share/cmigemo/utf-8/migemo-dict")

  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  (setq migemo-coding-system 'utf-8-unix)
  (load-library "migemo")
  (migemo-init)
)

;;; --- exec path from shell ---
;; disable to appear error message when using environment variables.
;; reference: https://stackoverflow.com/questions/35286203/exec-path-from-shell-message-when-starting-emacs
(setq exec-path-from-shell-check-startup-files nil)
;; load environment variables defined in shell.
(exec-path-from-shell-initialize)

;;; --- anzu (refactoring mode) ---
(use-package anzu
  :ensure
  :diminish anzu-mode
  :init
  (global-anzu-mode +1)
  :config
  (global-set-key [remap query-replace] 'anzu-query-replace)
  (global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)
  (setq anzu-deactivate-region t)
  (setq anzu-mode-lighter "")
  (setq anzu-replace-to-string-separator " => ")
  (setq anzu-replace-threshold 1000)
  (setq anzu-search-threshold 1000)
  (setq anzu-use-migemo t))

;;; --- auto complete
(require 'auto-complete)
(require 'auto-complete-config)
;;; delay setting
(setq ac-delay 0.0)
(setq ac-quick-help-delay 0.5)
;;; commands settting
(define-key ac-complete-mode-map "\C-n" 'ac-next)
(define-key ac-complete-mode-map "\C-p" 'ac-previous)
;;; global setting
(ac-config-default)
(global-auto-complete-mode t)

;;; --- rainbow-delimiters-mode
(use-package rainbow-delimiters
  :diminish rainbow-delimiters-mode
  :hook
  (prog-mode . rainbow-delimiters-mode))

;;; --- volatile-highlights-mode
;; (use-package volatile-highlights
;;   :diminish volatile-highlights-mode
;;   :hook
;;   (after-init . volatile-highlights-mode)
;;   :custom-face
;;   (vhl/default-face ((nil (:foreground "#FF3333" :background "#FFCDCD")))))

;;; --- symbol highlighting
;; reference: https://taipapamotohus.com/post/symbol-overlay/
(use-package symbol-overlay
  :diminish symbol-overlay-mode
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'symbol-overlay-mode)
  (add-hook 'markdown-mode-hook #'symbol-overlay-mode)
  (global-set-key (kbd "M-i") 'symbol-overlay-put)
  (define-key symbol-overlay-map (kbd "p") 'symbol-overlay-jump-prev)
  (define-key symbol-overlay-map (kbd "n") 'symbol-overlay-jump-next)
  (define-key symbol-overlay-map (kbd "C-g") 'symbol-overlay-remove-all))

;; reference: https://qiita.com/blue0513/items/99476f4ae51f17600636
(require 'diminish)
(eval-after-load "auto-complete" '(diminish 'auto-complete-mode))
(eval-after-load "eldoc" '(diminish 'eldoc-mode))
(eval-after-load "flycheck" '(diminish 'flycheck-mode))

;;; --- flycheck-mode
(use-package flycheck
  :ensure t
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enabled)))

;; Language Server
;; Note: go get golang.org/x/tools/gopls@latest
(use-package lsp-mode
  :diminish lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :custom ((lsp-inhibit-message t)
           (lsp-message-project-root-warning t)
           (create-lockfiles nil)))

;; Optional - provides fancier overlays.
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :custom
  ;; lsp-ui-doc
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature t)
  ;; top, bottom, or at-point
  (lsp-ui-doc-position 'top)
  (lsp-ui-doc-max-width 150)
  (lsp-ui-doc-max-height 30)
  (lsp-ui-doc-use-childframe t)
  (lsp-ui-doc-use-webkit t)
  ;; lsp-ui-flycheck
  (lsp-ui-flycheck-enable t)
  ;; lsp-ui-peek
  (lsp-ui-peek-enable t)
  (lsp-ui-peek-peek-height 20)
  (lsp-ui-peek-list-width 50)
  :hook (lsp-mode . lsp-ui-mode))

;; company-lsp integrates company mode completion with lsp-mode.
;; completion-at-point also works out of the box but doesn't support snippets.
(use-package company-lsp
  :ensure t
  :commands company-lsp
  :custom
  ;; always using cache
  (company-lsp-cache-candidates t)
  (company-lsp-async t)
  (company-lsp-enable-recompletion nil))

;; Company mode is a standard completion package that works well with lsp-mode.
;;; --- company-mode
(use-package company
  :diminish company-mode
  :ensure t
  :defer t
  :config
  ;; Optionally enable completion-as-you-type behavior.
  ;; reference1: https://qiita.com/kod314/items/3a31719db27a166d2ec1
  ;; reference2: https://qiita.com/blue0513/items/c0dc35a880170997c3f5
  ;; reference3: https://qiita.com/kai2nenobu/items/5dfae3767514584f5220
  (setq company-auto-expand t)
  (setq company-transformers '(company-sort-by-backend-importance))
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  (setq company-selection-wrap-around t)
  (setq completion-ignore-case t)
  (setq company-dabbrev-downcase nil)

  (global-set-key (kbd "C-M-i") 'company-complete)

  ;; Use quickhelp-mode
  (company-quickhelp-mode)

  ;; C-n and C-p selects auto completed words
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  ;; use C-n and C-p when search mode.
  ;; C-s filters words.
  (define-key company-search-map (kbd "C-n") 'company-select-next)
  (define-key company-search-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "C-s") 'company-filter-candidates)
  ;; use tab for completion
  (define-key company-active-map (kbd "C-i") 'company-complete-selection)
  (define-key company-active-map [tab] 'company-complete-selection)
  ;; C-h is disable for ackspace
  (define-key company-active-map (kbd "C-h") nil)
  ;; C-Shift-h shows document
  (define-key company-active-map (kbd "C-d") 'company-show-doc-buffer)

  ;; color setting.
  (set-face-attribute 'company-tooltip nil
                      :foreground "black"
                      :background "lightgray")
  (set-face-attribute 'company-preview-common nil
                      :foreground "dark gray"
                      :background "black"
                      :underline t)
  (set-face-attribute 'company-tooltip-selection nil
                      :background "steelblue"
                      :foreground "white")
  (set-face-attribute 'company-tooltip-common nil
                      :foreground "black"
                      :underline t)
  (set-face-attribute 'company-tooltip-common-selection nil
                      :foreground "white"
                      :background "steelblue"
                      :underline t)
  (set-face-attribute 'company-tooltip-annotation nil
                      :foreground "red"))


;;; --- go-mode
;; reference: https://github.com/golang/tools/blob/master/gopls/doc/emacs.md
;; reference: https://blog.web-apps.tech/lsp-mode-with-gopls/
;;
;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(use-package go-mode
  :ensure t
  :init (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
  :hook ((go-mode . lsp-deferred)
         (go-mode . company-mode)))

;;; --- python-mode
;;; extends python mode
(use-package python-mode
  :ensure t
  :config
	(setq indent-tab-mode nil)
	(setq indent-level 4)
	(setq python-indent 4)
	(setq tab-width 4))

(use-package elpy
  :ensure t
  :defer t
  :after (python-mode)
  :commands elpy-enable
  :init (elpy-enable)
  :hook ((elpy-mode . flycheck-mode)
         (python-mode . elpy-enable))
  :config
  (setq elpy-rpc-virtualenv-path 'current)
  ;; use jedi
	(setq elpy-rpc-backend "jedi")
  ;; set jupyter
  ;; https://elpy.readthedocs.io/en/latest/ide.html#interpreter-setup
  (setq python-shell-interpreter "jupyter")
  (setq python-shell-interpreter-args "console --simple-prompt")
  (setq python-shell-prompt-detect-failure-warning nil)
  (add-to-list 'python-shell-completion-native-disabled-interpreters
               "jupyter")

	(define-key company-active-map (kbd "\C-n") 'company-select-next)
	(define-key company-active-map (kbd "\C-p") 'company-select-previous)
	(define-key company-active-map (kbd "\C-d") 'company-show-doc-buffer)
	(define-key company-active-map (kbd "<tab>") 'company-complete)
  )

;; refactor tool
;; NOTE: pip install autopep8 before.
(use-package py-autopep8
  :ensure t
  :hook ((python-mode . py-autopep8-enable-on-save))
  :config
  (setq py-autopep8-options '("--max-line-length=80")))

;; fly check (flymake-python-pyflakes requires flymake-easy)
;; NOTE: pip install flake8 before.
(use-package flymake-easy
  :ensure t
  :defer t)
(use-package flymake-python-pyflakes
  :ensure t
  :after (flymake-easy)
  :hook ((python-mode . flymake-python-pyflakes-load))
  :config
  (setq flymake-python-pyflakes-executable "flake8"))

;;; --- yasnippet
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :bind (:map yas-minor-mode-map             
              ("C-x i i" . 'yas-insert-snippet)
              ("C-x i n" . 'yas-new-snippet)
              ("C-x i v" . 'yas-visit-snippet-file)
              ("C-x i l" . 'yas-describe-tables)
              ("C-x i g" . 'yas-reload-all))
  :config
  (yas-global-mode 1)
  (setq yas-prompt-functions '(yas-ido-prompt))
  (setq yas-snippet-dirs '("~/.emacs.d/yasnippets")))
  
;;; --- ruby-mode
(autoload 'ruby-mode "ruby-mode"
    "Mode for editing ruby source files" t)

;;; --- scala-mode
;; reference: https://scalameta.org/metals/docs/editors/emacs.html
(use-package scala-mode
  :mode "\\.s\\(cala\\|bt\\)$"
  :hook ((scala-mode . lsp-deferred)
         (scala-mode . flycheck-mode)))
(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
  ;; sbt-supershell kills sbt-mode:
  ;; https://github.com/hvesalai/emacs-sbt-mode/issues/152
  (setq sbt:program-options '("-Dsbt.supershell=false")))
;; reference: https://tarao.hatenablog.com/entry/metals
;; git clone https://github.com/tarao/scala-bootstrap-el.git
(require 'scala-bootstrap)

(add-hook 'scala-mode-hook
  '(lambda ()
     (scala-bootstrap:with-metals-installed
      (scala-bootstrap:with-bloop-server-started))))

;;; --- markdown mode
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode))
  :config
  (setq markdown-preview-stylesheets
        (list "https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/3.0.1/github-markdown.min.css"))
  (setq markdown-preview-javascript
        (list "https://cdnjs.cloudflare.com/ajax/libs/KaTeX/0.11.0/katex.min.js")))
;;
;; doesn't work.
;;
;; (add-to-list 'markdown-preview-stylesheets
;;              "https://cdnjs.cloudflare.com/ajax/libs/KaTeX/0.11.0/katex.min.css")
;; (add-to-list 'markdown-preview-javascript
;;              "https://cdnjs.cloudflare.com/ajax/libs/KaTeX/0.11.0/katex.min.js")
;; (add-to-list 'markdown-preview-javascript
;;              ("https://cdnjs.cloudflare.com/ajax/libs/KaTeX/0.11.0/contrib/auto-render.js")

;;; --- textlint flycheck
(flycheck-define-checker textlint
  "A linter for Markdown."
  :command ("textlint" "--format" "unix" source)
  :error-patterns
  ((warning line-start (file-name) ":" line ":" column ": "
     (id (one-or-more (not (any " "))))
     (message (one-or-more not-newline)
       (zero-or-more "\n" (any " ") (one-or-more not-newline)))
     line-end))
  :modes (text-mode markdown-mode gfm-mode))
 
(add-hook 'markdown-mode-hook
          '(lambda ()
             (setq flycheck-checker 'textlint)
             (flycheck-mode 1)))

;;; --- org-mode
(require 'org)
(require 'org-install)
;;; if 80 characters, goto next line.
(setq org-startup-trucated nil)
(setq org-return-follows-link t)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(setq org-directory "~/Org/")
(setq org-default-notes-files (concat org-directory "agenda.org"))
(setq org-agenda-files (list org-directory))

;;; --- others document mode
(use-package csv-mode
  :ensure t)
(use-package yaml-mode
  :ensure t)
(use-package dockerfile-mode
  :ensure t
  :mode ("Dockerfile\\'" . dockerfile-mode))

;;; plantuml-mode doesn't work.
;; reference: https://github.com/skuro/plantuml-mode
 (use-package plantuml-mode
   :init
   ;; NOTE: apt install plantuml
   (setq plantuml-options "-charset UTF-8")
   (setq plantuml-executable-path "/usr/bin/plantuml")
   (setq plantuml-default-exec-mode 'executable))

;; I don't like this indent
;; (use-package highlight-indent-guides
;;   :diminish
;;   :hook
;;   ((prog-mode yaml-mode) . highlight-indent-guides-mode)
;;   :custom
;;   (highlight-indent-guides-auto-enabled t)
;;   (highlight-indent-guides-responsive t)
;;   (highlight-indent-guides-method 'character))

;;; --- vue mode
(use-package vue-mode
  :ensure t
  :hook ((vue-mode . company-mode)
         (vue-mdoe . flycheck-mode)
         (vue-mode . eldoc-mode)
         (vue-mode . lsp-deferred))
  :config
  (setq js-indent-level 2)
	(setq css-indent-offset 2))
(use-package mmm-mode
  :ensure t
  :hook ((mmm-mode . company-mode)
         (mmm-mdoe . flycheck-mode)
         (mmm-mode . eldoc-mode))
  :config
  ;; (set-face-background 'mmm-default-submode-face "gray13")
	(setq indent-tab-mode nil)
  (setq mmm-submode-decoration-level 2)
  (setq tab-width 2)

  ;; Note: Should check by ESC-x regexp-builder.
  (mmm-add-classes
   '((vue-embeded-web-mode
      :submode vue-mode
      :front "^<template>\n"
      :back "</template>")
     (vue-embeded-js-mode
      :submode js-mode
      :front "^<script>\n"
      :back "^</script>")
     (vue-embeded-typescript-mode
      :submode typescript-mode
      :front "^<script.*lang=\"ts\".*>\n"
      :back "^</script>")
     (vue-embeded-css-mode
      :submode vue-mode
      :front "^<style>\n"
      :back "^</style>")
     (vue-embeded-scss-mode
      :submode scss-mode
      :front "^<style.*lang=\"scss\".*>"
      :back "^</style>")))

  (mmm-add-mode-ext-class nil "\\.vue\\'" 'vue-embeded-web-mode)
  (mmm-add-mode-ext-class nil "\\.vue\\'" 'vue-embeded-js-mode)
  (mmm-add-mode-ext-class nil "\\.vue\\'" 'vue-embeded-typescript-mode)
  (mmm-add-mode-ext-class nil "\\.vue\\'" 'vue-embeded-css-mode)
  (mmm-add-mode-ext-class nil "\\.vue\\'" 'vue-embeded-scss-mode))

;; Fix js-mode and typescript-mode indent into mmm-mode(vue-mode).
;; Reference: https://github.com/AdamNiederer/vue-mode/issues/74#issuecomment-539711083
(setq mmm-js-mode-enter-hook (lambda () (setq syntax-ppss-table nil)))
(setq mmm-typescript-mode-enter-hook (lambda () (setq syntax-ppss-table nil)))

;;; --- typescript mode
(use-package typescript-mode
  :ensure t
  :config
  (setq typescript-indent-level 2)
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode)))
;; need to install company and tide.
(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (typescript-mode . company-mode)
         (typescript-mdoe . flycheck-mode)
         (typescript-mode . eldoc-mode)
         (typescript-mode . lsp-deferred)
         (before-save . tide-format-before-save))
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enabled)))

;;; --- Web mode (php, pl, js, html)
(require 'web-mode)
(defun web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-html-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-script-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq indent-tabs-mode nil)
  (setq tab-width 2)
)
(add-hook 'web-mode-hook 'web-mode-hook)
;; set auto closing pairing
(setq web-mode-auto-close-style 1)
(setq web-mode-tag-auto-close-style t)
(setq web-mode-enable-auto-pairing t)
;; set mode alist
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . web-mode))

;;; R-mode
;;(add-to-list 'load-path "~/usr/share/emacs/site-lisp/ess")
;;(setq auto-mode-alist
;;	  (cons (cons "\\.r$" 'R-mode) auto-mode-alist))
;;(autoload 'R-mode "ess-site" "Emacs Speaks Statistics mode" t)
;;(setq ess-ask-for-ess-directory nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     OPAM configuration       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Add opam emacs directory to the load-path
(setq opam-share (substring (shell-command-to-string "opam config var share 2> /dev/null") 0 -1))
(add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))
;;; Load merlin-mode
;;; find shared library installed opam (no neccesarry?)
(let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
  (when (and opam-share (file-directory-p opam-share))
	(add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))))
(require 'merlin)
;;; Start merlin on ocaml files
(autoload 'merlin-mode "merlin" "Merlin mode" t)
(add-hook 'tuareg-mode-hook 'merlin-mode t)
(add-hook 'caml-mode-hook 'merlin-mode t)
;;; Enable auto-complete
(setq merlin-use-auto-complete-mode 'easy)
;;; (setq merlin-use-auto-complete-mode t)
;;; Use opam switch to lookup ocamlmerlin binary
(setq merlin-command 'opam)

;;; use the opam installed utop
(require 'utop)
(require 'utop-custom)
(setq utop-command "opam config exec -- utop -emacs")
;;; Automatically load utop.el
(autoload 'utop "utop" "Toplevel for OCaml" t)
;;; minor mode
(autoload 'utop-minor-mode "utop" "Minor mode for utop" t)

;;; flymake mode
(require 'flymake)
(push '("File \"\\(.*\\)\", line \\([0-9]+\\), characters \\([0-9]+\\)--?\\([0-9]+\\):\\(.*\\)" 1 2 3 5) flymake-err-line-patterns)
(push '("\\.ml\\'" flymake-ocaml-init) flymake-allowed-file-name-masks)
(defun flymake-ocaml-init ()
  (list (expand-file-name "~/.emacs.d/ocaml-flymake.sh")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  End of OPAM configuration   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; --- flymake-tuareg
;;(require 'flymake-tuareg)
;;(add-hook 'tuareg-mode-hook 'flymake-tuareg-load)

;;; --- tuareg
(use-package tuareg
  :ensure t
  :mode (("\\.ml[iylp]?$" . tuareg-mode)
         ("\\.eliom$" . tuareg-mode))
  :hook ((tuareg-mode . lsp-deferred)
         (tuareg-mode . utop-minor-mode)
         (tuareg-mode . company-mode)))
;; auto format by using ocamlformat
;; reference: https://github.com/ocaml-ppx/ocamlformat
(use-package ocamlformat
  :ensure t
  :after tuareg
  :hook ((before-save . ocamlformat-before-save))
  :config
  (define-key tuareg-mode-map (kbd "C-M-<tab>") 'ocamlformat))

;;; Set language 2
;;; Must put bottom !!!
(set-coding-system-priority 'utf-8)
(set-default-coding-systems 'utf-8)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-preview ((t (:foreground "darkgray" :underline t))))
 '(company-preview-common ((t (:inherit company-preview))))
 '(company-tooltip ((t (:background "lightgray" :foreground "black"))))
 '(company-tooltip-common ((((type x)) (:inherit company-tooltip :weight bold)) (t (:inherit company-tooltip))))
 '(company-tooltip-common-selection ((((type x)) (:inherit company-tooltip-selection :weight bold)) (t (:inherit company-tooltip-selection))))
 '(company-tooltip-selection ((t (:background "steelblue" :foreground "white"))))
 '(vhl/default-face ((nil (:foreground "#FF3333" :background "#FFCDCD")))))
(put 'upcase-region 'disabled nil)

;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line

;;; --- satysfi mode
;;(require 'satysfi)
;;(add-to-list 'auto-mode-alist '("\\.saty$" . satysfi-mode))
;;(add-to-list 'auto-mode-alist '("\\.satyh$" . satysfi-mode))
;;(add-hook 'satysfi-mode '(lambda () (setq tab-widh 2 indent-tab-mode nil)))
;;(setq satysfi-command "satysfi")
  ;; set the command for typesetting (default: "satysfi -b")
;;(setq satysfi-pdf-viewer-command "sumatrapdf")
  ;; set the command for opening PDF files (default: "open")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(package-selected-packages
   '(yaml-mode which-key web-mode vue-mode volatile-highlights use-package tuareg tide symbol-overlay swap-regions swap-buffers smex scss-mode scala-mode sbt-mode ruby-refactor ruby-electric ruby-block rainbow-delimiters python-mode py-autopep8 proof-general plantuml-mode php-mode php-completion paredit ocp-indent ocamlformat markdown-preview-mode markdown-preview-eww lsp-ui jedi ipython ido-vertical-mode ido-select-window ido-migemo ido-completing-read+ highlight-indent-guides go-eldoc go-dlv go-complete flymake-python-pyflakes exec-path-from-shell elpy dockerfile-mode diminish csv-mode company-quickhelp-terminal company-lsp coffee-fof cake2 cake auto-indent-mode anzu ac-nrepl)))
