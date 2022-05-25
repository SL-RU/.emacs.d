;; packages-autoinstall.el -- autoinstall all packages

;;; Commentary:
;;; autoinstall all packages
;;; Code:


(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))

(require 'use-package)

(use-package helm
  :ensure t
  :bind (("M-x"     . helm-M-x)
         ("C-x r b" . helm-filtered-bookmarks)
         ("C-x b"   . helm-buffers-list)
         ("C-x C-f" . helm-find-files)
         ("M-y"     . helm-show-kill-ring)
         ("C-c i d" . helm-complete-file-name-at-point)
         ("C-s"     . helm-swoop)
         :map helm-map
         ("C-d"       . helm-c-bookmark-run-delete)
         ("<tab>"     . helm-execute-persistent-action)
         ("<backtab>" . helm-find-files-up-one-level)
         ("C-i"       . helm-execute-persistent-action)
         ("C-z"       . helm-select-action)
         :map minibuffer-local-map
         ("M-p" . helm-minibuffer-history)
         ("M-n" . helm-minibuffer-history))
  :config
  (setq history-delete-duplicates t)
  ;; open helm buffer inside current window, not occupy whole other window
  (setq helm-split-window-in-side-p           t
        ;; move to end or beginning of source when reaching top or bottom of source.
        helm-move-to-line-cycle-in-source     t
        ;; search for library in `require' and `declare-function' sexp.
        helm-ff-search-library-in-sexp        t
        ;; ; scroll 8 lines other window using M-<next>/M-<prior>
        helm-scroll-amount                    8
        helm-ff-file-name-history-use-recentf t
        helm-echo-input-in-header-line t)
  (helm-mode 1))

(use-package helm-swoop
  :ensure t
  :after (helm))

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  (setq projectile-enable-caching t)
  (setq projectile-enable-cmake-presets t)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))

(use-package drag-stuff
  :ensure t
  :config
  (drag-stuff-define-keys)
  (drag-stuff-global-mode))
;; higlight cursors when scroll
(use-package beacon
  :ensure t
  :config
  (beacon-mode 1))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))
(use-package yasnippet-snippets
  :ensure t)

(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->"         . mc/mark-next-like-this)
         ("C-<"         . mc/mark-previous-like-this)
         ("C-c C-<"     . mc/mark-all-like-this)
         ("C-C C-v"     . uncomment-region)))
(use-package function-args
  :ensure t
  :config
  (fa-config-default))
(use-package nyan-mode
  :ensure t
  :config
  (nyan-mode 1))
(use-package switch-window
  :ensure t
  :bind (("C-x o" . switch-window)))
(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode))
(use-package tramp              :ensure t)
(use-package 2048-game          :ensure t)
(use-package ac-c-headers       :ensure t)
(use-package ac-slime           :ensure t)
(use-package beacon             :ensure t)
(use-package sudo-edit          :ensure t)

(use-package dired-quick-sort   :ensure t)
(use-package dired-single
  :ensure t
  :config
  (require 'dired-single)
  (defun my-dired-init ()
    "Bunch of stuff to run for dired, either immediately or when it's loaded."
    (define-key dired-mode-map [remap dired-find-file]
      'dired-single-buffer)
    (define-key dired-mode-map [remap dired-mouse-find-file-other-window]
      'dired-single-buffer-mouse)
    (define-key dired-mode-map [remap dired-up-directory]
      'dired-single-up-directory))
  ;; if dired's already loaded, then the keymap will be bound
  (if (boundp 'dired-mode-map)
      ;; we're good to go; just add our bindings
      (my-dired-init)
    ;; it's not loaded yet, so add our bindings to the load-hook
    (add-hook 'dired-load-hook 'my-dired-init)))

(use-package company
  :ensure t
  :config
  (setq company-show-numbers t)
  (setq company-tooltip-align-annotations t)
  (setq company-tooltip-flip-when-above t)
  (global-company-mode))
(use-package company-quickhelp
  :ensure t
  :init
  (company-quickhelp-mode 1)
  (use-package pos-tip
    :ensure t))
(use-package company-lsp
  :commands company-lsp
  :config (setq company-lsp-cache-candidates 'auto))

(use-package cmake-mode         :ensure t)
(use-package dot-mode           :ensure t)
(use-package ducpel             :ensure t)
(use-package flx-ido            :ensure t)
(use-package free-keys          :ensure t)
(use-package iedit              :ensure t)
(use-package image+             :ensure t)
(use-package image-dired+       :ensure t)
(use-package intel-hex-mode     :ensure t)
(use-package magit              :ensure t)
(use-package monokai-theme      :ensure t)

(use-package rainbow-identifiers :ensure t)
(use-package rustic             :ensure t)
(use-package smooth-scrolling   :ensure t)
(use-package sr-speedbar        :ensure t)
(use-package visual-fill-column :ensure t)
(use-package vlf                :ensure t)
(use-package xwidgete           :ensure t)
(use-package async              :ensure t)
(use-package yaml-mode          :ensure t)

;(use-package latex              :ensure t)
;(use-package reftex             :ensure t)
;(use-package auctex             :ensure t)
;(use-package auctex-latexmk             :ensure t)

(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode 1)
  (add-hook 'typescript-mode-hook 'flycheck-mode))

(use-package web-mode
  :ensure t
  :mode (("\\.html?\\'" . web-mode)
         ("\\.tsx\\'" . web-mode)
         ("\\.jsx\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-block-padding 2
        web-mode-comment-style 2

        web-mode-enable-css-colorization t
        web-mode-enable-auto-pairing t
        web-mode-enable-comment-keywords t
        web-mode-enable-current-element-highlight t
        )
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
		(setup-tide-mode))))
  (flycheck-add-mode 'typescript-tslint 'web-mode))
(use-package typescript-mode
  :ensure t
  :config
  (setq typescript-indent-level 2)
  (add-hook 'typescript-mode #'subword-mode))
(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)))

(use-package elpy
  :ensure t
  :init
  (elpy-enable))
(use-package py-autopep8
  :ensure t
  :hook (python-mode-hook . py-autopep8-enable-on-use))
(use-package python-black       :ensure t)

(use-package lsp-mode
  :ensure
  :commands lsp
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-idle-delay 0.3)
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-signature-auto-activate nil)
  (lsp-eldoc-enable-hover nil)
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))
(use-package lsp-ui
  :ensure
  :commands lsp-ui-mode
  :custom
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-doc-enable t)
  (lsp-ui-peek-a nil)
  )

(use-package go-mode
  :ensure t
  :bind (
         ;; If you want to switch existing go-mode bindings to use lsp-mode/gopls instead
         ;; uncomment the following lines
         ;; ("C-c C-j" . lsp-find-definition)
         ;; ("C-c C-d" . lsp-describe-thing-at-point)
         )
  :hook ((go-mode . lsp-deferred)
         (before-save . lsp-format-buffer)
         (before-save . lsp-organize-imports)))

(use-package lammps-mode
  :ensure t
  :mode (;;("in\\." . lammps-mode)
         ("\\.lmp\\'" . lammps-mode)))

;;; packages-autoinstall.el ends here
