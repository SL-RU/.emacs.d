;;; init --- My init file for emacs
;;; Commentary:
;;; It is always WIP
;;; Code:

(setq pgtk-wait-for-event-timeout nil)
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
(add-to-list 'default-frame-alist '(font . "Hack 13" ))
(setq bidi-paragraph-direction t)
(setq bidi-inhibit-bpa t)
(setq visible-bell 1)

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

;; LSP performance
(setq read-process-output-max (* 10 1024 1024)) ; for lsp
(setq max-lisp-eval-depth 10000)
(setq gc-cons-threshold 400000000)
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
                     text-mode-hook
                     js-mode-hook
                     rust-mode-hook
                     cmake-mode-hook
                     toml-mode-hook
                     lisp-mode-hook
                     elisp-mode-hook
                     emacs-lisp-mode-hook
                     typescript-mode-hook

                     web-mode-hook

                     c-ts-mode-hook
                     c++-ts-mode-hook
                     text-mode-hook
                     js-ts-mode-hook
                     rust-ts-mode-hook
                     cmake-ts-mode-hook
                     toml-ts-mode-hook
                     lisp-ts-mode-hook
                     elisp-ts-mode-hook
                     emacs-lisp-ts-mode-hook
                     typescript-ts-mode-hook
                     python-mode-hook
                     python-ts-mode-hook
                     sh-mode
                     ))
  (add-hook mode-hook #'display-line-numbers-mode))

(put 'erase-buffer 'disabled nil)
(put 'set-goal-column 'disabled nil)

(unless (display-graphic-p)
  (load-file (concat user-emacs-directory "wl-clipboard.el"))
  (xterm-mouse-mode t)
  )

(defun projectile--cmake-version ()
  "Compute CMake version."
  (let* ((string (shell-command-to-string "cmake --version"))
         (match (string-match "^cmake version \\([0-9.]+\\).*$" string)))
    (when match
      (version-to-list (match-string 1 string)))))

(defun insert-file-name (file &optional relativep)
  "Read RELATIVEP FILE name and insert it at point.
With a prefix argument, insert only the non-directory part."
  (interactive "fFile: \nP")
  (when relativep (setq file  (file-name-nondirectory file)))
  (insert file))


(defun insert-file-name-relative (file &optional relativep)
  "Read RELATIVEP FILE name and insert relative it at point.
With a prefix argument, insert only the non-directory part."
  (interactive "fFile: \nP")
  (when relativep (setq file  (file-name-nondirectory file)))
  (let ((fname (file-name-directory  (directory-file-name (buffer-file-name)))))
    (insert (file-relative-name file fname))))

;(select-frame frame)
;;; init.el ends here

