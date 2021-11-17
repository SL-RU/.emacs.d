;; packages-autoinstall.el -- autoinstall all packages

;;; Commentary:
;;; autoinstall all packages
;;; Code:


(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))

(require 'use-package)

(use-package 2048-game          :ensure t)
(use-package ac-c-headers       :ensure t)
(use-package ac-slime           :ensure t)
(use-package arduino-mode       :ensure t)
(use-package beacon             :ensure t)
                                        ;(use-package bookmark+          :ensure t)
                                        ;(use-package c-eldoc            :ensure t)
(use-package company
  :ensure t
  :config
  (setq company-show-numbers t)
  (setq company-tooltip-align-annotations t)
  (setq company-tooltip-flip-when-above t)
  (global-company-mode))
(use-package cmake-mode         :ensure t)
(use-package company-irony      :ensure t)
(use-package company-irony-c-headers :ensure t)
;(use-package dired+             :ensure t)
(use-package dired-quick-sort   :ensure t)
(use-package dired-single       :ensure t)
(use-package dot-mode           :ensure t)
(use-package drag-stuff         :ensure t)
(use-package ducpel             :ensure t)
(use-package elpy               :ensure t)
;(use-package flappymacs         :ensure t)
(use-package flx-ido            :ensure t)
(use-package flycheck
  :ensure t
  :config
  (add-hook 'typescript-mode-hook 'flycheck-mode))
(use-package flycheck-irony     :ensure t)
;(use-package flycheck-rtags     :ensure t)
(use-package free-keys          :ensure t)
(use-package function-args      :ensure t)
;(use-package header2            :ensure t)
;(use-package helm-rtags         :ensure t)
(use-package helm-swoop         :ensure t)
(use-package iedit              :ensure t)
(use-package image+             :ensure t)
(use-package image-dired+       :ensure t)
(use-package intel-hex-mode     :ensure t)
(use-package irony-eldoc        :ensure t)
(use-package magit              :ensure t)
;(use-package mongo              :ensure t)
(use-package monokai-theme      :ensure t)
(use-package multiple-cursors   :ensure t)
(use-package nyan-mode          :ensure t)
(use-package py-autopep8        :ensure t)
(use-package python-black       :ensure t)
(use-package rainbow-identifiers :ensure t)
(use-package rustic             :ensure t)
(use-package smooth-scrolling   :ensure t)
(use-package sr-speedbar        :ensure t)
(use-package switch-window      :ensure t)
;(use-package unbound            :ensure t)
(use-package undo-tree          :ensure t)
(use-package visual-fill-column :ensure t)
(use-package vlf                :ensure t)
(use-package web-mode           :ensure t)
(use-package xwidgete           :ensure t)
(use-package yasnippet          :ensure t)
(use-package yasnippet-snippets :ensure t)
;(use-package zone-nyan          :ensure t)
(use-package async              :ensure t)

;(use-package latex              :ensure t)
(use-package reftex             :ensure t)
;(use-package auctex             :ensure t)
;(use-package auctex-latexmk             :ensure t)

(use-package company-quickhelp
  :ensure t
  :init
  (company-quickhelp-mode 1)
  (use-package pos-tip
    :ensure t))

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
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))


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

(use-package company-lsp
  :commands company-lsp
  :config (setq company-lsp-cache-candidates 'auto))

;;; packages-autoinstall.el ends here
