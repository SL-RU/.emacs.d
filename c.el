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
  (auto-fill-mode)         
  (c-toggle-auto-hungry-state 1))

(add-hook 'c++-mode-hook 'my-c++-mode-hook)

(require 'rtags)
(cmake-ide-setup)

(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
;
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))
;
(require 'flycheck-clang-analyzer)
(flycheck-clang-analyzer-setup)
;
(add-hook 'c-mode-hook #'flycheck-irony-setup)
(require 'flycheck-irony)
(eval-after-load 'flycheck
  '(add-to-list 'flycheck-checkers 'irony))
(eval-after-load 'flycheck
  '(add-to-list 'flycheck-checkers 'clang-analyzer))

(require 'helm-rtags)

(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
(setq company-backends (delete 'company-semantic company-backends))
(eval-after-load 'company
  '(add-to-list
    'company-backends '(company-irony-c-headers company-irony)))

(setq company-idle-delay 0)
;(define-key c-mode-map [(tab)] 'company-complete)
;(define-key c++-mode-map [(tab)] 'company-complete)

(add-hook 'c++-mode-hook 'flycheck-mode)
(add-hook 'c-mode-hook 'flycheck-mode)

