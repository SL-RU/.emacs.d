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
(use-package cape
  :ensure t
)
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

(use-package helm-swoop
  :ensure t
  :after (helm))
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

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
;(use-package function-args
;  :ensure t
;  :config
;  (fa-config-default))
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
  (setq company-backends '(company-capf company-dabbrev-code))
  (setq company-tooltip-align-annotations t)
  (setq company-tooltip-flip-when-above t)
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
        web-mode-enable-current-element-highlight t
        ))
  ;(add-hook 'web-mode-hook
            ;(lambda ()
             ; (add-to-list 'write-file-functions 'delete-trailing-whitespace)
              ;(when (string-equal "tsx" (file-name-extension buffer-file-name))
                                        ;(setup-tide-mode))
            ;(flycheck-add-mode 'typescript-tslint 'web-mode))
(use-package typescript-mode
  :ensure t
  :config
  (setq typescript-indent-level 2)
  (add-hook 'typescript-mode-hook
            (lambda () (add-to-list 'write-file-functions 'delete-trailing-whitespace)))
  (add-hook 'typescript-mode #'subword-mode))
;(use-package tide
;  :ensure t
;  :after (typescript-mode company flycheck)
;  :hook ((typescript-mode . tide-setup)
;         (typescript-mode . tide-hl-identifier-mode)))

;(use-package elpy
;  :ensure t
;  :init
;  (elpy-enable))
;(use-package py-autopep8
;  :ensure t
;  :hook (python-mode-hook . py-autopep8-enable-on-use))
;(use-package python-black       :ensure t)


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
  :ensure t
  :commands lsp
  :bind-keymap ("C-c l" . lsp-command-map)
  :hook ((go-mode . lsp)
         (c-mode . lsp)
         (c++-mode . lsp)
         (typescript-mode . lsp)
         (javascript-mode . lsp)
         (js-mode . lsp)
         (web-mode . lsp)
         (python-mode . lsp)
         (markdown-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-idle-delay 0.3)
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-signature-auto-activate nil)
  (lsp-eldoc-enable-hover nil)
  (lsp-enable-indentation nil)
  (push 'company-lsp company-backends)
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  (setq lsp-idle-delay 0.1))
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :custom
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-doc-enable t)
  (lsp-ui-peek-a nil)
  )

(use-package dap-mode
  :straight '(dap-mode
              :type git
              :host github
              :branch "feat/cortex-debug"
              :repo "mrsch/dap-mode")
  :config
  (defun counsel-fzf-rg:org ()
    (interactive)
    (counsel-fzf-rg "" org-directory))
  :bind (("C-c n f". counsel-fzf-rg:org)))

(use-package dap-mode
  :ensure t
  :config
  (require 'dap-cpptools)
  (require 'dap-gdb-lldb)
  (setq dap-auto-configure-features '(sessions locals controls tooltip))
  (dap-mode 1)
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (tooltip-mode 1)
  (dap-ui-controls-mode 1)
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
         (go-mode . (lambda ()
                      (add-to-list 'write-file-functions 'delete-trailing-whitespace)
                      (before-save . lsp-format-buffer)
                      (before-save . lsp-organize-imports)
                      ))))

(use-package lammps-mode
  :ensure t
  :mode (;;("in\\." . lammps-mode)
         ("\\.lmp\\'" . lammps-mode)))

(use-package ess
  :ensure t
  :init (require 'ess-site))
(use-package coterm
  :ensure t
  :init (coterm-mode))

(straight-use-package
 '(lsp-tailwindcss :type git :host github :repo "merrickluo/lsp-tailwindcss"))

(use-package lsp-tailwindcss
  :init
  (setq lsp-tailwindcss-add-on-mode t))

(use-package tree-sitter
  :ensure t
  :config
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter)

(use-package ts-fold
  :straight (ts-fold :type git :host github :repo "emacs-tree-sitter/ts-fold")
  :after tree-sitter-langs
  :bind (("C-c f o"   . ts-fold-open)
         ("C-c f c"   . ts-fold-close)
         ("C-c f r"   . ts-fold-open-recursively)
         ("C-c f a c" . ts-fold-open-all)
         ("C-c f a o" . ts-fold-close-all)
         ("C-c f t"   . ts-fold-toggle)))

(use-package helm-tree-sitter
  :ensure t
  :after tree-sitter
  :bind (("C-c C-f" . helm-tree-sitter)))

(use-package xenops
  :ensure t
  ;; :hook
  ;; (org-mode . xenops-mode)
  :bind
  ;; FIX: xenops overrides the default paste behavior with xenops-handle-paste through xenops-util-define-key-with-fallback in xenops-define-key which breaks the delete-selection-mode
  ;(:map xenops-mode-map
  ;        ("s-v" . yank))
  :custom
  (setq xenops-reveal-on-entry t)
  :config
  ;; Suppress xenops startup messages.
  (advice-add 'xenops-mode :around #'suppress-messages)
  (setq xenops-math-image-scale-factor 1.8))

(use-package avy
  :ensure t
  :bind
  (("C-:" . avy-goto-char-2)))

;;; packages-autoinstall.el ends here
