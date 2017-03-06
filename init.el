(require 'package) ;; You might already have this line
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
;(add-to-list 'package-archives
;             '("elpy" . "https://jorgenschaefer.github.io/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(load-file (concat user-emacs-directory "/cedet/cedet-devel-load.el"))
(load-file (concat user-emacs-directory "cedet/contrib/cedet-contrib-load.el"))

(add-to-list 'package-archives
             '("elpy" . "https://jorgenschaefer.github.io/packages/"))

(package-initialize) ;; You might already have this line

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

;;THEME
(load-theme 'monokai t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(add-to-list 'default-frame-alist '(font . "Hack" ))
(set-face-attribute 'default t :font "Hack 14" )
(set-face-attribute 'default nil :height 110)

(global-linum-mode t) ;; enable line numbers globally

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

(show-paren-mode) ;pair brackets

(require 'nyan-mode)
(nyan-mode 1)

(require 'switch-window)
(global-set-key (kbd "C-x o") 'switch-window)

(diredp-toggle-find-file-reuse-dir 1) ;;dired+


(require 'undo-tree)
(global-undo-tree-mode)


(require 'drag-stuff)
(drag-stuff-define-keys)
(drag-stuff-global-mode)

(require 'tramp)

(defun sudo-edit (&optional arg)
  "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(global-set-key (kbd "C-x C-r ") 'sudo-edit)


(require 'elpy)
(elpy-enable)
(setq elpy-rpc-python-command "python3")
(setq python-shell-interpreter "python3")
(add-to-list 'python-shell-completion-native-disabled-interpreters "python3")

(require 'auto-complete)
; do default config for auto-complete
(require 'auto-complete-config)
(ac-config-default)
; start yasnippet with emacs
(require 'yasnippet)
(yas-global-mode 1)

(require 'flycheck)
(global-flycheck-mode t)

(setq steam-username "sl_ru")

(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)


(load-file (concat user-emacs-directory "helm-init.el"))
(load-file (concat user-emacs-directory "ede-init.el"))

(load-file (concat user-emacs-directory "stm32/stm32.el"))
(load-file (concat user-emacs-directory "flycheck-cedet.el"))
;(require 'stm32)
(stm32-load-all-projects)

(defun my-ede-hook ()
  "hook for activating flycheck"
  (interactive)
  (flycheck-setup-from-cedet)
  )
(add-hook 'c-mode-common-hook 'my-ede-hook)

(defun my-flycheck-c-setup ()
  (setq flycheck-gcc-language-standard "gnu99"))
(add-hook 'c-mode-hook #'my-flycheck-c-setup)

(load-file (concat user-emacs-directory "c.el"))


;;you need to install rust, cargo, rust-racer
(require 'rust-mode)
(require 'racer)
(autoload 'rust-mode "rust-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
(setq racer-rust-src-path "~/.local/share/rust_src/src") ;DO: git clone --recursive https://github.com/rust-lang/rust.git ~/.local/share/rust_src
(setq racer-cmd "/usr/bin/racer")
;; (add-to-list 'load-path "<path-to-racer>/editors")
;;(add-hook 'rust-mode-hook #'racer-activate)
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(eval-after-load "rust-mode" '(require 'racer))
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-rust-setup))
(add-hook 'racer-mode-hook #'company-mode)
(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
(setq company-tooltip-align-annotations t)
