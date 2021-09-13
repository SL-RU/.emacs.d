;; tex.el -- initialize packages for latex

;;; Commentary:
;;; latex packages
;;; Code:

(require 'latex)
(require 'reftex)
(require 'company)

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

(add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)
(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
(setq TeX-source-correlate-method 'synctex)
(setq TeX-source-correlate-start-server t)

(setq TeX-view-program-selection '((output-pdf "Zathura")))
(add-hook 'LaTeX-mode-hook (lambda ()
                             (flyspell-mode)
                             (turn-on-reftex)))

(company-auctex-init)
(add-to-list 'company-backends 'company-reftex-labels)
(add-to-list 'company-backends 'company-reftex-citations)
;;; tex.el ends here
