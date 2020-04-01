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
  (auto-fill-mode))
  ;(c-toggle-auto-hungry-state 1))

(add-hook 'c++-mode-hook 'my-c++-mode-hook)

(require 'rtags)
(cmake-ide-setup)

(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)
;
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
;                                        ;
(require 'company)
(eval-after-load 'company
'(add-to-list 'company-backends 'company-irony))
                                        ;

(require 'flycheck-irony)
(add-hook 'c-mode-hook #'flycheck-irony-setup)
(eval-after-load 'flycheck
'(add-to-list 'flycheck-checkers 'irony))
;
(add-hook 'c-mode-hook
	  (lambda ()
	    (setq flycheck-clang-language-standard "c11")))

(require 'helm-rtags)
(require 'company-irony-c-headers)

(setq company-backends (delete 'company-semantic company-backends))
(eval-after-load 'company
 '(add-to-list
   'company-backends '(company-irony-c-headers company-irony)))


(define-key c-mode-map (kbd "C-c . c") 'cmake-ide-compile)
(define-key c++-mode-map (kbd "C-c . c") 'cmake-ide-compile)
(add-hook 'cmake-mode-hook
	  (lambda() (local-set-key (kbd "C-c . c") 'cmake-ide-compile)))

;;; c.el ends here
