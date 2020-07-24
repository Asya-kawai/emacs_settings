;;; emacs-lisp; coding
;;; -*- coding:utf-8 -*-
;;; Common Lisp extensions for Emacs
(require 'cl)


;;; set default mode to scrach buffer
(setq default-major-mode 'lisp-interaction-mode)

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
;(tool-bar-mode 0)
;(menu-bar-mode 0)
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

;;; Auto Fill Mode
;;(setq fill-column 80)
;;(setq-default auto-fill-mode -1)
;;(setq text-mode-hook 'turn-on-auto-fill)

;;; Time
(setq display-time-day-and-date t)
(display-time)
(setq display-time-string-forms
	'((format "%s/%s/%s(%s) %s:%s"
		year month day
		dayname
		24-hours minutes)
	))

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

;;; R-mode
;;(add-to-list 'load-path "~/usr/share/emacs/site-lisp/ess")
;;(setq auto-mode-alist
;;	  (cons (cons "\\.r$" 'R-mode) auto-mode-alist))
;;(autoload 'R-mode "ess-site" "Emacs Speaks Statistics mode" t)
;;(setq ess-ask-for-ess-directory nil)

;;; unformal repositories 'mepla' and 'marmalade'
;;; --- emacs package install ---
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
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
                      ocp-indent
                      ;flymake-tuareg
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
(setq ido-enable-flex-matching t) 
(setq ido-enable-dot-prefix t)

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
;; load environment variables defined in shell.
(exec-path-from-shell-initialize)

;;; --- anzu (refactoring mode) ---
(global-anzu-mode +1)
(setq anzu-use-migemo t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(anzu-deactivate-region t)
 '(anzu-mode-lighter "")
 '(anzu-replace-to-string-separator " => ")
 '(anzu-search-threshold 1000)
 '(avy-migemo-function-names
   (quote
    (swiper--add-overlays-migemo
     (swiper--re-builder :around swiper--re-builder-migemo-around)
     (ivy--regex :around ivy--regex-migemo-around)
     (ivy--regex-ignore-order :around ivy--regex-ignore-order-migemo-around)
     (ivy--regex-plus :around ivy--regex-plus-migemo-around)
     ivy--highlight-default-migemo ivy-occur-revert-buffer-migemo ivy-occur-press-migemo avy-migemo-goto-char avy-migemo-goto-char-2 avy-migemo-goto-char-in-line avy-migemo-goto-char-timer avy-migemo-goto-subword-1 avy-migemo-goto-word-1 avy-migemo-isearch avy-migemo-org-goto-heading-timer avy-migemo--overlay-at avy-migemo--overlay-at-full)))
 '(package-selected-packages
   (quote
    (diminish volatile-highlights highlight-indent-guides dockerfile-mode csv-mode symbol-overlay ido-completing-read+ ido-select-window ido-migemo ido-vertical-mode smex company-quickhelp-terminal which-key company-quickhelp go-eldoc company-lsp proof-general swap-buffers swap-regions gnu-elpa-keyring-update go-dlv yaml-mode markdown-preview-mode markdown-preview-eww tide typescript-mode lsp-ui use-package lsp-mode markdown-mode exec-path-from-shell go-complete go-mode flycheck web-mode vue-mode tuareg scss-mode ruby-refactor ruby-electric ruby-block rainbow-delimiters python-mode py-autopep8 php-mode php-completion paredit ocp-indent jedi ipython flymake-python-pyflakes elpy coffee-fof caml cake2 cake auto-indent-mode anzu ac-nrepl)))
 '(safe-local-variable-values (quote ((enconding . utf-8)))))

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

;;; --- go mode
(add-hook 'go-mode-hook 'flycheck-mode)

;; reference: https://github.com/golang/tools/blob/master/gopls/doc/emacs.md
;; reference: https://blog.web-apps.tech/lsp-mode-with-gopls/

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; Language Server
;; Note: go get golang.org/x/tools/gopls@latest
(use-package lsp-mode
  :diminish lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook (go-mode . lsp-deferred))

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
(add-hook 'go-mode-hook 'company-mode)

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
                      :foreground "red")
  )

;;; tuareg-mode
(setq auto-mode-alist
      (cons '("\\.ml[iylp]?$" . tuareg-mode) auto-mode-alist))
(add-to-list 'auto-mode-alist
      '("\\.eliom$" . tuareg-mode))
(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code." t)
(add-hook 'tuareg-mode-hook 'utop-minor-mode)

;;; --- flymake-tuareg
;;(require 'flymake-tuareg)
;;(add-hook 'tuareg-mode-hook 'flymake-tuareg-load)

;;; --- python-mode
;;; extends python mode
(add-hook 'python-mode-hook
					'(lambda ()
						 (setq indent-tab-mode nil)
						 (setq indent-level 4)
						 (setq python-indent 4)
						 (setq tab-width 4)
						 (package-initialize)
						 (elpy-enable)
						 (elpy-mode)
						 ))
(add-hook 'elpy-mode-hook
					'(lambda ()
						 (elpy-use-ipython)
						 ;;; fix prompt
						 (setenv "IPY_TEST_SIMPLE_PROMPT" "1")
						 ;;; quickrun C-c C-r
						 ;;(define-key elpy-mode-map "\C-c\C-r" 'quickrun)
						 ;;; disable auto complete mode
						 (auto-complete-mode -1)
						 ;;; use jedi
						 (setq elpy-rpc-backend "jedi")
						 (define-key company-active-map (kbd "\C-n") 'company-select-next)
						 (define-key company-active-map (kbd "\C-p") 'company-select-previous)
						 (define-key company-active-map (kbd "\C-d") 'company-show-doc-buffer)
						 (define-key company-active-map (kbd "<tab>") 'company-complete)
						 ))
;; set ipython
(setq python-shell-interpreter "ipython"
			python-shell-interpreter-args "-i")
;; refactor tool
;; NOTE: pip install autopep8 before.
(require 'py-autopep8)
(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
(setq py-autopep8-options '("--max-line-length=80"))
;; fly check (flymake-python-pyflakes requires flymake-easy)
;; NOTE: pip install flake8 before.
(require 'flymake-easy)
(require 'flymake-python-pyflakes)
(add-hook 'python-mode-hook 'flymake-python-pyflakes-load)
(setq flymake-python-pyflakes-executable "flake8")
;; pyvenv is switching some resource.
(pyvenv-mode 1)
(pyvenv-tracking-mode 1)

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
(use-package csv-mode)
(use-package yaml-mode)
(use-package dockerfile-mode
  :mode ("Dockerfile\\'" . dockerfile-mode))

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
(require 'vue-mode)
(require 'mmm-mode)
(defun my-vue-mode-hook ()
	"Hooks for vue mode."
	(setq js-indent-level 2)
	(setq css-indent-offset 2)
  (setq tab-width 2)
	(setq indent-tab-mode nil)
)
(add-hook 'vue-mode-hook 'my-vue-mode-hook)

;;; --- typescript mode

;; need to install typescript-mode.
(require 'typescript-mode)
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
;; need to install company and tide.
(require 'tide)
(add-hook 'typescript-mode-hook
          (lambda ()
            (tide-setup)
            (flycheck-mode t)
            (setq flycheck-check-syntax-automatically '(save mode-enabled))
            (eldoc-mode t)
            (company-mode-on)))

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

;;; --- markdown mode
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

;;; --- markdown preview mode
(setq markdown-preview-stylesheets
      (list "https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/3.0.1/github-markdown.min.css"))

(setq markdown-preview-javascript
      (list "https://cdnjs.cloudflare.com/ajax/libs/KaTeX/0.11.0/katex.min.js"))
;;
;; doesn't work.
;;
;; (add-to-list 'markdown-preview-stylesheets
;;              "https://cdnjs.cloudflare.com/ajax/libs/KaTeX/0.11.0/katex.min.css")
;; (add-to-list 'markdown-preview-javascript
;;              "https://cdnjs.cloudflare.com/ajax/libs/KaTeX/0.11.0/katex.min.js")
;; (add-to-list 'markdown-preview-javascript
;;              ("https://cdnjs.cloudflare.com/ajax/libs/KaTeX/0.11.0/contrib/auto-render.js")

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

;;; omake mode
;;(setq load-path
;;      (append '(expand-file-name
;;                "/usr/local/share/emacs/site-lisp/omake-mode") load-path))
;;(require 'omake)

;;; flymake mode
(require 'flymake)
(push '("File \"\\(.*\\)\", line \\([0-9]+\\), characters \\([0-9]+\\)--?\\([0-9]+\\):\\(.*\\)" 1 2 3 5) flymake-err-line-patterns)
(push '("\\.ml\\'" flymake-ocaml-init) flymake-allowed-file-name-masks)
(defun flymake-ocaml-init ()
  (list (expand-file-name "~/.emacs.d/ocaml-flymake.sh")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  End of OPAM configuration   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
(require 'satysfi)
(add-to-list 'auto-mode-alist '("\\.saty$" . satysfi-mode))
(add-to-list 'auto-mode-alist '("\\.satyh$" . satysfi-mode))
(add-hook 'satysfi-mode '(lambda () (setq tab-widh 2 indent-tab-mode nil)))
(setq satysfi-command "satysfi")
  ; set the command for typesetting (default: "satysfi -b")
(setq satysfi-pdf-viewer-command "sumatrapdf")
  ; set the command for opening PDF files (default: "open")

;;(require 'restclient-vscode-compatible)
