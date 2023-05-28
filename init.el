;;; init --- My init file for emacs
;;; Commentary:
;;; It is always WIP
;;; Code:

(server-start) ;; start server to open files in the same window
(setq native-comp-deferred-compilation-deny-list nil) ;; https://github.com/radian-software/straight.el/issues/767
(setq native-comp-deferred-compilation t)
(setq native-compile-prune-cache t)
;(setq global-linum-mode nil) ;; https://github.com/emacsorphanage/git-gutter/issues/221

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(add-to-list 'package-archives
             '("elpy" . "https://jorgenschaefer.github.io/packages/"))
(package-initialize)

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

(load-file (concat user-emacs-directory "packages-autoinstall.el"))

;;THEME
(load-theme 'monokai t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(add-to-list 'default-frame-alist '(font . "Hack 11" ))
(setq bidi-paragraph-direction t)
(setq bidi-inhibit-bpa t)
(setq visible-bell 1)


(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

(setq read-process-output-max (* 1024 1024)) ; for lsp
(setq max-lisp-eval-depth 10000)
(setq max-specpdl-size 32000)
(setq gc-cons-threshold 100000000)
(setq create-lockfiles nil)
;; pair brackets
(show-paren-mode)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
;; make indentation commands use space only (never tab character)
;; emacs 23.1, 24.2, default to t
;; if indent-tabs-mode is t, it means it may use tab, resulting mixed space and tab
(setq-default indent-tabs-mode nil)
(turn-off-auto-fill)

(load-file (concat user-emacs-directory "c.el"))
;(load-file (concat user-emacs-directory "tex.el"))
;;(load-file (concat user-emacs-directory "rust.el"))

(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(defun stm32-flash-to-mcu ()
  "Upload compiled binary to stm32 through gdb if gdb has been started."
  (interactive)
  (progn (gdb-io-interrupt)
         (gud-basic-call "load")
         (gud-basic-call "cont")))

(add-hook 'sh-mode-hook #'display-line-numbers-mode)
(add-hook 'c-mode-hook #'display-line-numbers-mode)
(add-hook 'c++-mode-hook #'display-line-numbers-mode)
(add-hook 'web-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)

(require 'tree-sitter)
(global-tree-sitter-mode)
(require 'ts-fold)
(global-ts-fold-mode)
(global-ts-fold-indicators-mode)

(setq lsp-tex-server 'digestif)

(put 'erase-buffer 'disabled nil)
(put 'set-goal-column 'disabled nil)
;;; init.el ends here
