;; c.el -- initialize packages for c & c++

;;; Commentary:
;;; c/c++ packages
;;; Code:

(load (concat user-emacs-directory "gendoxy/gendoxy.el"))

(load-file (concat user-emacs-directory "rtags/src/rtags.el"))
(load-file (concat user-emacs-directory "rtags/src/helm-rtags.el"))
(load-file (concat user-emacs-directory "rtags/src/flycheck-rtags.el"))

; style I want to use in c++ mode
(c-add-style "my-style"
	     '("stroustrup"
	       (indent-tabs-mode . nil)        ; use spaces rather than tabs
	       (c-basic-offset . 4)            ; indent by four spaces
	       (c-offsets-alist . ((inline-open . 0)  ; custom indentation rules
                                   (innamespace . [0])
				   (brace-list-open . 0)
                                   (brace-list-intro . ++)
                                   (member-init-intro . ++)
				   (statement-case-open . +)))))

(defun my-c++-mode-hook ()
  "C++ hook."
  (c-set-style "my-style") ; use my-style defined above
  (auto-fill-mode))
(add-hook 'c++-mode-hook 'my-c++-mode-hook)
(add-hook 'c-mode-hook 'my-c++-mode-hook)


(require 'rtags)
(add-hook 'c-mode-hook 'rtags-start-process-unless-running)
(add-hook 'c++-mode-hook 'rtags-start-process-unless-running)
(setq rtags-display-result-backend 'helm)
(define-key c-mode-map (kbd "C-x t f") 'rtags-find-symbol-at-point)
(define-key c-mode-map (kbd "C-x t r") 'rtags-find-all-references-at-point)
(define-key c-mode-map (kbd "C-c . c") 'stm32-make-build)
(define-key c-mode-map (kbd "C-c . f") 'stm32-flash-to-mcu)
(define-key c++-mode-map (kbd "C-x t f") 'rtags-find-symbol-at-point)
(define-key c++-mode-map (kbd "C-x t r") 'rtags-find-all-references-at-point)
(define-key c++-mode-map (kbd "C-c . c") 'stm32-make-build)
(define-key c++-mode-map (kbd "C-c . f") 'stm32-flash-to-mcu)
;(add-hook 'kill-emacs-hook 'rtags-quit-rdm)

(use-package irony
  :ensure t
  :hook ((c++-mode . irony-mode)
         (c-mode . irony-mode)
         (objc-mode . irony-mode)
         (irony-mode . irony-cdb-autosetup-compile-options)))
(use-package irony-eldoc
  :ensure t
  :hook ((irony-mode . irony-eldoc)))
(use-package company-irony
  :ensure t
  :after (company)
  :config
  (add-to-list 'company-backends 'company-irony))
;; Load with `irony-mode` as a grouped backend
(use-package company-irony-c-headers
  :ensure t
  :after (company)
  :config
  '(add-to-list
    'company-backends '(company-irony-c-headers company-irony)))

(use-package flycheck-irony
  :ensure t
  :after (flycheck)
  :hook ((flycheck-mode . flycheck-irony-setup))
  :config
  (add-to-list 'flycheck-checkers 'irony)
  (defun setup-flycheck-var ()
    "RTags create more accurate overlays."
    (setq flycheck-clang-language-standard "gnu17"))
  (add-hook 'c-mode-hook #'setup-flycheck-var)
  (add-hook 'c++-mode-hook #'setup-flycheck-var))

(load-file (concat user-emacs-directory "stm32/stm32.el"))
(defun rtags-add-project-from-irony ()
  "Add rtags project using build directory found by irony in stm32.el."
  (interactive)
  (let ((dep-buffer (rtags-get-buffer "*RTags add project*"))
        (arg (stm32-get-project-build-dir)))
    (message (format "Adding build dir %s to rtags" arg))
    (rtags-delete-rtags-windows)
    (rtags-location-stack-push)
    (rtags-switch-to-buffer dep-buffer)
    (rtags-call-rc "-J" arg)
    (rtags-mode)))

;;; c.el ends here
