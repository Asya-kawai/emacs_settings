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
;;; --- emacs package install ---
(defvar my-packages '(
                      ;;; --- refactoring ---
                      anzu
                      ;;; --- auto-complete ---
                      auto-complete
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
                      ;;; --- vue mode ---
                      vue-mode
                      mmm-mode
                      vue-html-mode
                      ssass-mode
                      edit-indirect
                      ;;; --- go mode ---
                      go-mode
                      go-autocomplete
                      go-eldoc
                      go-dlv
                      ;;; --- scss mode ---
                      scss-mode
                      ;;; --- web mode ---
                      web-mode
                      ;;; --- haml mode ---
                      haml-mode
                      ;;; --- coffee sript ---
                      coffee-mode
                      coffee-fof
                      ;;; --- php mode ---
                      php-mode
                      php-completion
                      helm
                      helm-gtags
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
                      ;;; flymake-tuareg
                      ;;; --- markdown mode ---
                      markdown-mode
                      ;;; --- org mode ---
                      org
                      ;;; --- lsp(language server protocol) mode ---
                      lsp-mode
                      ))
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-refresh-contents)
    (package-install p)))

;;; --- exec path from shell ---
;; load environment variables defined in shell.
(exec-path-from-shell-initialize)

;;; --- anzu (refactoring mode) ---
(global-anzu-mode +1)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(anzu-deactivate-region t)
 '(anzu-mode-lighter "")
 '(anzu-replace-to-string-separator " => ")
 '(anzu-search-threshold 1000)
 '(package-selected-packages
   (quote
    (go-dlv yaml-mode markdown-preview-mode markdown-preview-eww company tide typescript-mode lsp-ui lsp-go use-package lsp-mode go-eldoc markdown-mode exec-path-from-shell go-complete company-go go-mode flycheck web-mode vue-mode tuareg scss-mode ruby-refactor ruby-electric ruby-block rainbow-delimiters python-mode py-autopep8 php-mode php-completion paredit ocp-indent jedi ipython helm-gtags haml-mode flymake-python-pyflakes elpy coffee-fof caml cake2 cake auto-indent-mode anzu ac-nrepl)))
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

;;; --- go mode
;; reference: https://emacs-jp.github.io/programming/golang
(with-eval-after-load 'go-mode
   ;; auto-complete
   (require 'go-autocomplete)
   ;; flycheck and save-hook
   (add-hook 'go-mode-hook 'flycheck-mode)
   (add-hook 'before-save-hook 'gofmt-before-save)
   ;; eldoc
   (add-hook 'go-mode-hook 'go-eldoc-setup))

(use-package lsp-mode
  :custom ((lsp-inhibit-message t)
         (lsp-message-project-root-warning t)
         (create-lockfiles nil))
  :hook   (prog-major-mode . lsp-prog-major-mode-enable))

(use-package lsp-ui
  :after lsp-mode
  :custom (scroll-margin 0)
  :hook   (lsp-mode . lsp-ui-mode))

(use-package company-lsp
  :after (lsp-mode company yasnippet)
  :defines company-backends
  :functions company-backend-with-yas
  :init (cl-pushnew (company-backend-with-yas 'company-lsp) company-backends))

(use-package lsp-go
  :after (lsp-mode go-mode)
  :custom (lsp-go-language-server-flags '(
    "-gocodecompletion"
    "-diagnostics"
    "-lint-tool=golint"))
  :hook (go-mode . lsp-go-enable)
  :commands lsp-go-enable)

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
;;; set ipython
(setq python-shell-interpreter "ipython"
			python-shell-interpreter-args "-i")
;;; refactor tool
;;; NOTE: pip install autopep8 before.
(require 'py-autopep8)
(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
(setq py-autopep8-options '("--max-line-length=80"))
;;; fly check (flymake-python-pyflakes requires flymake-easy)
;;; NOTE: pip install flake8 before.
(require 'flymake-easy)
(require 'flymake-python-pyflakes)
(add-hook 'python-mode-hook 'flymake-python-pyflakes-load)
(setq flymake-python-pyflakes-executable "flake8")
(require 'yasnippet)
(yas-global-mode 1)
;;; pyvenv is switching some resource.
(pyvenv-mode 1)
(pyvenv-tracking-mode 1)

;;; --- ruby-mode
(autoload 'ruby-mode "ruby-mode"
    "Mode for editing ruby source files" t)

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
;; customize company mode for typescript.
(require 'company)
;; to choice C-n, C-p.
(define-key company-active-map (kbd "M-n") nil)
(define-key company-active-map (kbd "M-p") nil)
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
;; reset C-h from default mapping.
(define-key company-active-map (kbd "C-h") nil)
;; use tab for select item from map.
(define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
;; show document.
(define-key company-active-map (kbd "M-d") 'company-show-doc-buffer)

(setq company-minimum-prefix-length 1)
(setq company-selection-wrap-around t)

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
;;; set auto closing pairing
(setq web-mode-auto-close-style 1)
(setq web-mode-tag-auto-close-style t)
(setq web-mode-enable-auto-pairing t)
;;; 
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . web-mode))

;;; --- HAML mode
(require 'haml-mode)
(add-to-list 'auto-mode-alist '("\\.haml?\\'" . haml-mode))
(add-hook 'haml-mode-hook
		  (lambda ()
			(setq indent-tabs-mode nil)
			(define-key haml-mode-map "\C-m" 'newline-and-indent)))

;;; markdown mode
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

;;; markdown preview mode
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
 '(company-tooltip-selection ((t (:background "steelblue" :foreground "white")))))
(put 'upcase-region 'disabled nil)
;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line

;; --- satysfi mode
(require 'satysfi)
(add-to-list 'auto-mode-alist '("\\.saty$" . satysfi-mode))
(add-to-list 'auto-mode-alist '("\\.satyh$" . satysfi-mode))
(add-hook 'satysfi-mode '(lambda () (setq tab-widh 2 indent-tab-mode nil)))
(setq satysfi-command "satysfi")
  ; set the command for typesetting (default: "satysfi -b")
(setq satysfi-pdf-viewer-command "sumatrapdf")
  ; set the command for opening PDF files (default: "open")
