;; packages-autoinstall.el -- autoinstall all packages

;;; Commentary:
;;; autoinstall all packages
;;; Code:

(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))

;; Install straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(require 'use-package)

;; Add extensions
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
  (setq helm-split-window-inside-p           t
        ;; move to end or beginning of source when reaching top or bottom of source.
        helm-move-to-line-cycle-in-source     t
        ;; ; scroll 8 lines other window using M-<next>/M-<prior>
        helm-scroll-amount                    8
        helm-echo-input-in-header-line t)
  (helm-mode 1))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

(use-package helm-icons
  :ensure t
  :config
  (setq helm-icons-provider 'nerd-icons)
  (helm-icons-enable))

(use-package helm-swoop
  :ensure t
  :after (helm))
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

(use-package zoom
  :ensure t
  :init
  (zoom-mode 't))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :init
  (column-number-mode)
  (require 'doom-modeline-segments)
  (defun doom-modeline-lsp-icon (text face)
    "Display LSP icon (or TEXT in terminal) with FACE."
    (if doom-modeline-lsp-icon
        (doom-modeline-icon 'faicon "nf-fa-cat" "" text :face face)
      (propertize text 'face face))))

(use-package nerd-icons
  :straight (nerd-icons
             :type git
             :host github
             :repo "rainstormstudio/nerd-icons.el"
             :files (:defaults "data"))
  :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  (nerd-icons-font-family "Symbols Nerd Font Mono")
  )

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  (setq projectile-enable-caching t)
  (setq projectile-enable-cmake-presets t)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)))

(use-package drag-stuff
  :ensure t
  :config
  (drag-stuff-define-keys)
  (drag-stuff-global-mode))
;; higlight cursors when scroll
;; (use-package beacon
;;   :ensure t
;;   :config
;;   (beacon-mode 1))

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
         ("C-c C-<"     . mc/mark-all-like-this)))

;; ;; ;(use-package function-args
;; ;; ;  :ensure t
;; ;; ;  :config
;; ;; ;  (fa-config-default))

(use-package switch-window
  :ensure t
  :bind (("C-x o" . switch-window)))

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode))

(use-package dired-quick-sort   :ensure t)

(use-package company
  :ensure t
  :config
  (setq company-backends '(company-capf company-dabbrev-code))
  (setq company-tooltip-align-annotations t)
  (setq company-tooltip-flip-when-above t)
  (setq company-global-modes '(not erc-mode message-mode eshell-mode gud-mode))
  (setq company-transformers nil)
  (global-company-mode))

(use-package company-quickhelp
  :ensure t
  :init
  (company-quickhelp-mode 1)
  (use-package pos-tip
    :ensure t))

(use-package cmake-mode
  :ensure t
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'")
  :hook (cmake-mode . lsp-deferred))
(use-package cmake-font-lock
  :ensure t
  :after cmake-mode
  :config (cmake-font-lock-activate))
(use-package dockerfile-mode
  :ensure t
  :hook (dockerfile-mode . lsp))
(use-package ducpel             :ensure t)
(use-package free-keys          :ensure t)
(use-package iedit              :ensure t)
(use-package image+             :ensure t)
(use-package image-dired+       :ensure t)
(use-package intel-hex-mode     :ensure t)
(use-package magit              :ensure t)
;; (use-package monokai-theme      :ensure t)
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-monokai-classic t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  ; (doom-themes-neotree-config)
  ;; or for treemacs users
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package solaire-mode :ensure t
  :init
  (solaire-global-mode +1))
(use-package rainbow-identifiers :ensure t)
(use-package sr-speedbar        :ensure t)
(use-package visual-fill-column :ensure t)
(use-package vlf                :ensure t)
(use-package xwidgete           :ensure t)
(use-package async              :ensure t)
(use-package yaml-mode          :ensure t)

(use-package reftex             :ensure t)
(use-package auctex             :ensure t)
(use-package auctex-latexmk             :ensure t)

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
        web-mode-enable-current-element-highlight t
        ))
(use-package typescript-mode
  :ensure t
  ;:after tree-sitter
  :config
  (setq typescript-indent-level 2)
  (add-hook 'typescript-mode-hook
            (lambda () (add-to-list 'write-file-functions 'delete-trailing-whitespace)))
  (add-hook 'typescript-mode #'subword-mode)
  (define-derived-mode typescriptreact-mode typescript-mode
    "TypeScript TSX")

  ;; use our derived mode for tsx files
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescriptreact-mode))
  ;; by default, typescript-mode is mapped to the treesitter typescript parser
  ;; use our derived mode to map both .tsx AND .ts -> typescriptreact-mode -> treesitter tsx
  ;; (add-to-list 'tree-sitter-major-mode-language-alist '(typescriptreact-mode . tsx)))

(use-package which-key
  :ensure t
  :defer 10
  :config
  (setq which-key-popup-type 'side-window) ;Default
  ;; (setq which-key-popup-type 'minibuffer)
  (setq which-key-compute-remaps t) ;Show correct descriptions for remapped keys

  (setq which-key-allow-multiple-replacements t) ;Default = nil
  ;; Use cool unicode characters if available
  (with-eval-after-load 'setup-font-check
    (when font-symbola-p
      (add-to-list 'which-key-replacement-alist '((nil . "\\`calc-") . (nil . "🖩")))
      (add-to-list 'which-key-replacement-alist '((nil . "\\`engine/search-") . (nil . "🔎 "))))) ;engine-mode

  ;; Change what string to display for a given *complete* key binding
  ;; Eg: After "C-x", display "8 → +unicode" instead of "8 → +prefix"
  (which-key-add-key-based-replacements
    "C-x 8"   "unicode"
    "C-x a"   "abbrev/expand"
    "C-x r"   "rectangle/register/bookmark"
    "C-x v"   "version control"
    "C-c /"   "engine-mode-map"
    "C-c C-v" "org-babel"
    "C-x 8 0" "ZWS")

  ;; Highlight certain commands
  (defface modi/which-key-highlight-2-face
    '((t . (:inherit which-key-command-description-face :foreground "indian red")))
    "Another face for highlighting commands in `which-key'.")

  (defface modi/which-key-highlight-3-face
    '((t . (:inherit which-key-command-description-face :foreground "DarkOrange3")))
    "Another face for highlighting commands in `which-key'.")
  (setq which-key-highlighted-command-list
        '(("\\`hydra-" . which-key-group-description-face)
          ;; Highlight using the `modi/which-key-highlight-2-face'
          ("\\`modi/" . modi/which-key-highlight-2-face)
          ;; Highlight using the `modi/which-key-highlight-3-face'
          ("\\`bookmark-" . modi/which-key-highlight-3-face)
          ("\\`counsel-" . modi/which-key-highlight-3-face)
          ;; Highlight using the default `which-key-highlighted-command-face'
          "\\`describe-"
          "\\(rectangle-\\)\\|\\(-rectangle\\)"
          "\\`org-"))
  (which-key-mode 1))

(use-package lsp-mode
  :straight '(lsp-mode
              :type git :host github :repo "emacs-lsp/lsp-mode"
              )
  :ensure t
  :commands lsp
  :bind-keymap ("C-c l" . lsp-command-map)
  :hook ((go-mode . lsp)
         (c-mode . lsp)
         (c++-mode . lsp)
         (c-or-c++-mode . lsp)
         (typescript-mode . lsp)
         (js-mode . lsp)
         (python-mode . lsp)
         (rust-mode . lsp)
         (toml-mode . lsp)
         (yaml-mode . lsp)

         (markdown-mode . lsp)
         (web-mode . lsp)

         (go-ts-mode . lsp)
         (c-ts-mode . lsp)
         (c++-ts-mode . lsp)
         (c-or-c++-ts-mode . lsp)
         (typescript-ts-mode . lsp)
         (js-ts-mode . lsp)
         (python-ts-mode . lsp)
         (rust-ts-mode . lsp)
         (toml-ts-mode . lsp)
         (yaml-ts-mode . lsp)

         (lsp-mode . lsp-enable-which-key-integration))
  :custom
  (require 'lsp-icons)
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (setq lsp-prefer-flymake nil)
  (lsp-idle-delay 0.5)
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-signature-auto-activate nil)
  (lsp-eldoc-enable-hover nil)
  (lsp-enable-indentation nil)
  (setq lsp-pylsp-plugins-isort-enabled t)
  (setq lsp-pylsp-plugins-black-enabled t)
  (setq lsp-pylsp-plugins-pylint-enabled t)
  (setq lsp-pylsp-plugins-autopep8-enabled t)
  (setq lsp-pylsp-plugins-pycodestyle-enabled t)
  (push 'company-lsp company-backends)
  (setq lsp-ui-doc-enable nil)
  (setq lsp-log-io nil) ; if set to true can cause a performance hit
  )

(use-package dap-mode
  :ensure t
  :config
  (require 'dap-cpptools)
  (require 'dap-gdb-lldb)
  (setq dap-default-terminal-kind "integrated") ;; Make sure that terminal programs open a term for I/O in an Emacs buffer
                                        ;(dap-mode 1)
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (tooltip-mode 1)
  (dap-auto-configure-mode +1)
  (dap-ui-controls-mode 1)
  )

(use-package go-mode
  :ensure t
  :hook ((go-mode . lsp-deferred)
         (go-mode . (lambda ()
                      (add-to-list 'write-file-functions 'delete-trailing-whitespace)
                      (before-save . lsp-format-buffer)
                      (before-save . lsp-organize-imports)
                      ))))

(use-package lammps-mode
  :ensure t
  :mode (;;("in\\." . lammps-mode)
         ("\\.lmp\\'" . lammps-mode)))

(use-package detached
  :ensure t
  :init (detached-init)
  :bind (
         ;; Replace `async-shell-command' with `detached-shell-command'
         ([remap async-shell-command] . detached-shell-command)
         ;; Replace `compile' with `detached-compile'
         ([remap compile] . detached-compile)
         ([remap recompile] . detached-compile-recompile))
  ;; Replace built in completion of sessions with `consult'
  ([remap detached-open-session] . detached-consult-session)
  :custom ((detached-show-output-on-attach t)
           (detached-terminal-data-command system-type)))

(use-package vterm
  :ensure t)

(straight-use-package
 '(lsp-tailwindcss :type git :host github :repo "merrickluo/lsp-tailwindcss"))

(use-package lsp-tailwindcss
  :init
  (setq lsp-tailwindcss-add-on-mode t))

(use-package helm-lsp
  :ensure t
  :init
  (define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol)
  )

(use-package lsp-treemacs
  :ensure t
  :init
  (lsp-treemacs-sync-mode 1)
  (treemacs-follow-mode t)
  (treemacs-tag-follow-mode t)
  (treemacs-project-follow-mode t)
  )

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

; простая навигация по символам.
(use-package avy
  :ensure t
  :bind
  (("C-:" . avy-goto-char-2)))

;; для работы самописных комманд
(use-package friendly-shell-command
  :ensure t
  )
(use-package s
  :ensure t
  )

(use-package fzf
  :ensure t
  :bind
  (("C-c p c" . fzf-find-file))
  ;; Don't forget to set keybinds!
  :config
  (setq fzf/args "-x --print-query --margin=1,0 --no-hscroll"
        fzf/executable "fzf"
        fzf/git-grep-args "-i --line-number %s"
        ;; command used for `fzf-grep-*` functions
        ;; example usage for ripgrep:
        ;; fzf/grep-command "rg --no-heading -nH"
        fzf/grep-command "grep -nrH"
        ;; If nil, the fzf buffer will appear at the top of the window
        fzf/position-bottom t
        fzf/window-height 15))

(use-package highlight-indent-guides
  :ensure t
  :hook ((prog-mode text-mode conf-mode) . highlight-indent-guides-mode)
  :init
  (setq highlight-indent-guides-method (if (display-graphic-p) 'bitmap 'character)
        highlight-indent-guides-bitmap-function #'highlight-indent-guides--bitmap-line)
  )

; сложные сочетания клавиш, когда емакс работает в терминале
(use-package kkp
  :ensure t
  :config
  ;; (setq kkp-alt-modifier 'alt) ;; use this if you want to map the Alt keyboard modifier to Alt in Emacs (and not to Meta)
  (global-kkp-mode +1))

(use-package lua-mode
  :ensure t
  :config
  (autoload 'lua-mode "lua-mode" "Lua editing mode." t)
  (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
  (add-to-list 'interpreter-mode-alist '("lua" . lua-mode)))


(use-package rust-mode
  :ensure t
  :init
                                        ; You can try the new native treesitter mode rust-ts-mode with:
  (setq rust-mode-treesitter-derive nil)
  )

;;; packages-autoinstall.el ends here
