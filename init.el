;;; init --- My init file for emacs
;;; Commentary:
;;; It is always WIP
;;; Code:

(server-start) ;; start server to open files in the same window

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(add-to-list 'package-archives
             '("elpy" . "https://jorgenschaefer.github.io/packages/"))
(package-initialize)

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

(load-file (concat user-emacs-directory "packages-autoinstall.el"))

;;THEME
(load-theme 'monokai t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(add-to-list 'default-frame-alist '(font . "Hack 16" ))
(setq bidi-paragraph-direction t)
(setq bidi-inhibit-bpa t)
(setq visible-bell 1)
;; enable line numbers globally
(global-linum-mode t)

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

(setq max-lisp-eval-depth 10000)
(setq max-specpdl-size 32000)
(set 'gc-cons-threshold 100000000)
(setq create-lockfiles nil)
;; pair brackets
(show-paren-mode)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
;; make indentation commands use space only (never tab character)
;; emacs 23.1, 24.2, default to t
;; if indent-tabs-mode is t, it means it may use tab, resulting mixed space and tab
(setq-default indent-tabs-mode nil)

(load-file (concat user-emacs-directory "c.el"))
;(load-file (concat user-emacs-directory "tex.el"))
;;(load-file (concat user-emacs-directory "rust.el"))

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

;;; init.el ends here
