;;; init --- My init file for emacs
;;; Commentary:
;;; It is always WIP
;;; Code:

(setq comp-deferred-compilation t)
(setq pgtk-wait-for-event-timeout nil)
(setq native-comp-async-jobs-number 1)
(defvar native-comp-deferred-compilation-deny-list nil)
(server-start) ;; start server to open files in the same window

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
(setq-default tab-width 2)

;; (load-file (concat user-emacs-directory "cpp-init.el"))
(load-file (concat user-emacs-directory "stm32/stm32.el"))
(load-file (concat user-emacs-directory "colorpick.el"))
(load-file (concat user-emacs-directory "google-c-style.el"))
(load-file (concat user-emacs-directory "gendoxy/gendoxy.el"))
(load-file (concat user-emacs-directory "my.el"))
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

(dolist (pattern '("\\.phtml\\'"
                   "\\.tpl\\.php\\'"
                   "\\.[agj]sp\\'"
                   "\\.as[cp]x\\'"
                   "\\.erb\\'"
                   "\\.mustache\\'"
                   "\\.djhtml\\'"
                   "\\.html?\\'"))
  (add-to-list 'auto-mode-alist (cons pattern 'web-mode)))

(dolist (mode-hook '(sh-mode-hook
                     c-mode-hook
                     c++-mode-hook
                     web-mode-hook
                     text-mode-hook
                     js-mode-hook
                     rust-mode-hook
                     cmake-mode-hook
                     toml-mode-hook
                     lisp-mode-hook
                     elisp-mode-hook
                     emacs-lisp-mode-hook
                     typescript-mode-hook))
  (add-hook mode-hook #'display-line-numbers-mode))

(put 'erase-buffer 'disabled nil)
(put 'set-goal-column 'disabled nil)

(defun my-gdbmi-bnf-target-stream-output (c-string)
  "Change behavior for GDB/MI targe the target-stream-output C-STRING so that it is displayed to the console."
  (gdb-console c-string))

(advice-add 'gdbmi-bnf-target-stream-output :override 'my-gdbmi-bnf-target-stream-output)

(unless (display-graphic-p)
  (load-file (concat user-emacs-directory "wl-clipboard.el"))
  (xterm-mouse-mode t)
  )

;;; init.el ends here

