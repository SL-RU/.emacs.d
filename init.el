;;; init --- My init file for emacs
;;; Commentary:
;;; It is always WIP
;;; Code:

(server-start) ;; start server to open files in the same window
(require 'package) ;; You might already have this line
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
;(add-to-list 'package-archives
;             '("elpy" . "https://jorgenschaefer.github.io/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

;(load-file (concat user-emacs-directory "/cedet/cedet-devel-load.el"))
;(load-file (concat user-emacs-directory "cedet/contrib/cedet-contrib-load.el"))

(add-to-list 'package-archives
             '("elpy" . "https://jorgenschaefer.github.io/packages/"))

(package-initialize) ;; You might already have this line

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

(load-file (concat user-emacs-directory "packages-autoinstall.el"))

;;THEME
(load-theme 'monokai t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(add-to-list 'default-frame-alist '(font . "Hack 16" ))

(setq visible-bell 1)
(global-linum-mode t) ;; enable line numbers globally

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

(setq max-lisp-eval-depth 10000)
(setq max-specpdl-size 32000)

(set 'gc-cons-threshold 100000000)

(show-paren-mode) ;pair brackets

(require 'nyan-mode)
(nyan-mode 1)

(require 'switch-window)
(global-set-key (kbd "C-x o") 'switch-window)

                                        ;(diredp-toggle-find-file-reuse-dir 1) ;;dired+
;(add-hook 'dired-load-hook
;          (lambda ()
            ;(load "dired-x")
            ;; Set dired-x global variables here.  For example:
            ;; (setq dired-guess-shell-gnutar "gtar")
            ;; (setq dired-x-hands-off-my-keys nil)
;            ))
;(add-hook 'dired-mode-hook
;          (lambda ()
            ;; Set dired-x buffer-local variables here.  For example:
            ; (dired-omit-mode 1)
;            ))


(require 'undo-tree)
(global-undo-tree-mode)


(require 'drag-stuff)
(drag-stuff-define-keys)
(drag-stuff-global-mode)

(require 'tramp)

(require 'beacon)
(beacon-mode 1)

(require 'iedit)

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

(put 'upcase-region 'disabled nil)

(require 'company)
(require 'py-autopep8)
(require 'elpy)
(elpy-enable)
;(require 'company-jedi)
;(setq python-shell-interpreter "python3")
;(defun my/python-mode-hook ()
;  "Python autocomplete."
;  (add-to-list 'company-backends 'company-jedi))
;(add-hook 'python-mode-hook 'my/python-mode-hook)
;(setq jedi:complete-on-dot t)                 ; optional
;(add-hook 'python-mode-hook 'flycheck-mode)
;(add-hook 'python-mode-hook 'python-black-on-save-mode)
(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)

; start yasnippet with emacs
(require 'yasnippet)
(yas-global-mode 1)

(require 'flycheck)
(global-flycheck-mode t)

(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(global-set-key (kbd "C-C C-v") 'uncomment-region)


(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'after-init-hook 'global-company-mode)

;(with-eval-after-load 'company
;  (global-set-key (kbd "<backtab>") 'company-complete))

(load-file (concat user-emacs-directory "helm-init.el"))
;(load-file (concat user-emacs-directory "c_lsp.el"))
(load-file (concat user-emacs-directory "c.el"))
(load-file (concat user-emacs-directory "rust.el"))

(setq auto-mode-alist (cons '("\\.\\(pde\\|ino\\)$" . arduino-mode) auto-mode-alist))
(autoload 'arduino-mode "arduino-mode" "Arduino editing mode." t)


(progn
  ;; make indentation commands use space only (never tab character)
  (setq-default indent-tabs-mode nil)
  ;; emacs 23.1, 24.2, default to t
  ;; if indent-tabs-mode is t, it means it may use tab, resulting mixed space and tab
  )
(put 'downcase-region 'disabled nil)

(require 'dired-single)
(defun my-dired-init ()
 "Bunch of stuff to run for dired, either immediately or when it's
  loaded."
 ;; <add other stuff here>
 (define-key dired-mode-map [remap dired-find-file]
   'dired-single-buffer)
 (define-key dired-mode-map [remap dired-mouse-find-file-other-window]
   'dired-single-buffer-mouse)
 (define-key dired-mode-map [remap dired-up-directory]
   'dired-single-up-directory))

;if dired's already loaded, then the keymap will be bound
(if (boundp 'dired-mode-map)
   ;; we're good to go; just add our bindings
   (my-dired-init)
 ;; it's not loaded yet, so add our bindings to the load-hook
 (add-hook 'dired-load-hook 'my-dired-init))



(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(defun my-dpi (&optional frame)
  "Get the DPI of FRAME (or current if nil)."
  (cl-flet ((pyth (lambda (w h)
                    (sqrt (+ (* w w)
                             (* h h)))))
            (mm2in (lambda (mm)
                     (/ mm 25.4))))
    (let* ((atts (frame-monitor-attributes frame))
           (pix-w (cl-fourth (assoc 'geometry atts)))
           (pix-h (cl-fifth (assoc 'geometry atts)))
           (pix-d (pyth pix-w pix-h))
           (mm-w (cl-second (assoc 'mm-size atts)))
           (mm-h (cl-third (assoc 'mm-size atts)))
           (mm-d (pyth mm-w mm-h)))
      (/ pix-d (mm2in mm-d)))))

(defvar my-dpi-last 0)
(defun my-zoom-frm-by-dpi (&optional frame)
  "Zoom FRAME so the DPI is closer to `my-zoom-frm-wanted-dpi'."
  (interactive)
  (when (frame-size-changed-p frame)
    (let ((dpi (truncate (my-dpi nil))))
      ;;(message (format "DPI: %d" dpi))
      (when (not (eq my-dpi-last dpi))
        (message (format "DPI: %d" dpi))
        (setq my-dpi-last dpi)
        (if (< dpi 110)
            (set-face-attribute 'default nil :height 120)
          (set-face-attribute 'default nil :height 170))))))
(add-hook 'window-size-change-functions #'my-zoom-frm-by-dpi)

(load-file (concat user-emacs-directory "tex.el"))


(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1)
  (setq create-lockfiles nil))


(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))
;; enable typescript-tslint checker
(flycheck-add-mode 'typescript-tslint 'web-mode)

(require 'tide)
(add-hook 'js2-mode-hook #'setup-tide-mode)
;; configure javascript-tide checker to run after your default javascript checker
(flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append)

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "jsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))
;; configure jsx-tide checker to run after your default jsx checker
(flycheck-add-mode 'javascript-eslint 'web-mode)
(flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append)

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

(projectile-mode +1)
;(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

(with-eval-after-load 'tide
  (flycheck-add-mode 'typescript-tslint 'ng2-ts-mode)
  (flycheck-add-mode 'typescript-tide 'ng2-ts-mode)
)

;(require 'dap-node)
;(dap-mode 1)

;; The modes below are optional
;(dap-ui-mode 1)
;; enables mouse hover support
;(dap-tooltip-mode 1)
;; use tooltips for mouse hover
;; if it is not enabled `dap-mode' will use the minibuffer.
;(tooltip-mode 1)
;; displays floating panel with debug buttons
;; requies emacs 26+
;(dap-ui-controls-mode 1)

;(dap-register-debug-template
;  "Nodmon::Run"
;  (list :type "node"
;        :request "attach"
;        :restart t
;        :port 9229
;        :name "Nodmon::Run"))
;
;(setq lsp-clients-angular-language-server-command
;  '("node"
;    "/usr/lib/node_modules/@angular/language-server"
;    "--ngProbeLocations"
;    "/usr/lib/node_modules"
;    "--tsProbeLocations"
;    "/usr/lib/node_modules"
;    "--stdio"))

;(with-eval-after-load 'typescript-mode (add-hook 'typescript-mode-hook #'lsp))

;;; init.el ends here
