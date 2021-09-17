;;;; Author: 

;;; set http sources instead of https.

(require 'package)
(setq package-archives '(("gnu" . "http://elpa.emacs-china.org/gnu/")
			 ("melpa" . "http://elpa.emacs-china.org/melpa/")))
(package-initialize)

;;; copied from wiki, set charset to display UTF-8 Chinese characters correctly with LANG=C.

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-buffer-file-coding-system 'utf-8-unix)
(set-clipboard-coding-system 'utf-8-unix)
(set-file-name-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(set-next-selection-coding-system 'utf-8-unix)
(set-selection-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(setq locale-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;;; misc configuration.

(setq make-backup-files nil)
(setq auto-save-default nil)
(setq-default indent-tabs-mode nil)
(setq indent-tabs-mode nil)
(menu-bar-mode -1)
(setq-default c-basic-offset 4)
(setq confirm-kill-emacs 'y-or-n-p)
(setenv "PAGER" "/bin/cat")

;; high light parentheses
(show-paren-mode t)
(setq show-paren-style 'parentheses)

;;; managed by emacs.

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (scala-mode cmake-mode ace-window bazel-mode dockerfile-mode protobuf-mode highlight-parentheses yaml-mode flymake rust-mode company-lsp web-mode gnu-elpa-keyring-update evil yasnippet magit go-mode lsp-ui use-package lsp-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;; use-package modules.

(require 'use-package)

;;; built-in modules.

(use-package ido
  :ensure t
  :config (ido-mode t))

(use-package man
  :ensure t
  :config (progn (set-face-attribute 'Man-overstrike
                                     nil
                                     :inherit 'bold
                                     :foreground "blue")
                 (set-face-attribute 'Man-underline
                                     nil
                                     :inherit 'underline
                                     :foreground "magenta")))

;;; 3rd-party modules.

(use-package lsp-mode
  ;; we also have: `lsp-clients` `lsp-ui` packages.
  :hook ((c++-mode c-mode) . lsp)
  :config (progn (setq lsp-enable-on-type-formatting nil))
  :commands lsp)

(use-package lsp-ui
  :after (lsp-mode)
  :config (progn (set-face-attribute 'lsp-ui-sideline-current-symbol
                                     nil
                                     :foreground "brightblue")
                 (set-face-attribute 'lsp-ui-sideline-symbol-info
                                     nil
                                     :foreground "brightblack")
                 (set-face-attribute 'lsp-ui-doc-background
                                     nil
                                     :background "brightblack")
                 (setq lsp-ui-sideline-enable nil)))

(use-package magit
  :bind ("C-x g" . magit-status))

(use-package ace-window
  :bind ("C-x o" . ace-window))

(use-package evil
  :ensure t
  :config (progn (dolist (mode '('dired-mode
				 'eshell-mode
				 'scratch-mode
				 'help-mode))
		   (evil-set-initial-state mode 'emacs))))

(use-package yaml-mode)

(use-package web-mode
  :mode ("\\.html\\'"
	 "\\.js\\'"
	 "\\.ts\\'"
	 "\\.jsx\\'"
	 "\\.tsx\\'"
	 "\\.vue\\'"
	 "\\.css\\'"))

(use-package bazel-mode
  :mode ("BUILD"
         "WORKSPACE"
         "\\.bzl\\'"))

;;; key bindings

;; <f5> through <f9> can be used.
;;(global-set-key (kbd "<f5>") (lambda ()
;;			       (interactive)
;;				 (revert-buffer t t)))
;;(global-set-key (kbd "<f6>") 'lsp-find-declaration)
;;(global-set-key (kbd "<f7>") 'lsp-execute-code-action)
;;(global-set-key (kbd "<f8>") 'lsp-format-buffer)
;;(global-set-key (kbd "<f9>") 'compile)

;;; open c coresponding header-impl file.
(load-file "~/.emacs.d/lisp/wcy-c-open-other-file.el")
(global-set-key (kbd "C-x t") 'wcy-c-open-other-file)

(global-set-key (kbd "M-i") 'find-file-at-point)

;;; bindings.

(setq grep-command "fgrep --exclude-dir=.git --exclude=*.pbtxt --exclude=*.pyc -nHi -R . -e ")

(global-set-key (kbd "C-x C-m") 'compile)
(setq compile-command "bazel build //")
