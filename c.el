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
				   (brace-list-open . 0)
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

(require 'irony)
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(add-hook 'irony-mode-hook #'irony-eldoc)

(require 'company)
(require 'company-irony)
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))
(require 'company-irony-c-headers)
; Load with `irony-mode` as a grouped backend
(eval-after-load 'company
  '(add-to-list
    'company-backends '(company-irony-c-headers company-irony)))

(require 'flycheck-irony)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))
(eval-after-load 'flycheck
  '(add-to-list 'flycheck-checkers 'irony))
(defun setup-flycheck-var ()
  "RTags create more accurate overlays."
  (setq flycheck-clang-language-standard "gnu17"))
(add-hook 'c-mode-hook #'setup-flycheck-var)
(add-hook 'c++-mode-hook #'setup-flycheck-var)

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
