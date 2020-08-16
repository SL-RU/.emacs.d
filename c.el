;; c.el -- initialize packages for c & c++

;;; Commentary:
;;; lol
;;; Code:

(setq c-default-style "linux"
      c-basic-offset 4)

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
  (c-toggle-auto-hungry-state 1)
  (auto-fill-mode))
(add-hook 'c++-mode-hook 'my-c++-mode-hook)


(require 'flycheck-irony)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))
(eval-after-load 'flycheck
'(add-to-list 'flycheck-checkers 'irony))

(require 'rtags)
(add-hook 'c-mode-hook 'rtags-start-process-unless-running)
(add-hook 'c++-mode-hook 'rtags-start-process-unless-running)
(setq rtags-display-result-backend 'helm)
(global-set-key (kbd "C-x t f") 'rtags-find-symbol-at-point)
(global-set-key (kbd "C-x t r") 'rtags-find-all-references-at-point)

(defun setup-flycheck-var ()
  "RTags create more accurate overlays."
  (setq flycheck-clang-language-standard "c11"))
(add-hook 'c-mode-hook #'setup-flycheck-var)
(add-hook 'c++-mode-hook #'setup-flycheck-var)


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
;; Load with `irony-mode` as a grouped backend
(eval-after-load 'company
  '(add-to-list
    'company-backends '(company-irony-c-headers company-irony)))


;;; c.el ends here
