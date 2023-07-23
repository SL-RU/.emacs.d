;; colorpick.el --- color chooser

;;; Commentary:
;;;
;;; Code:

(require 'friendly-shell-command)
(require 's)

(defun colorpick--type (index)
  "Pick color using grim and get color by INDEX."
  (let* ((out (s-split
               "\n"
               (friendly-shell-command-to-string
                "grim -g \"$(slurp -p)\" -t ppm - | convert - -format '%[pixel:p{0,0}]' txt:-")))
         (words (s-split
                 " "
                 (nth 1 out))))
    (nth index words)))

(defun colorpick-hex ()
  "Pick color using grim print as hex; #XXXXXX."
  (interactive)
  (insert (colorpick--type 3)))

(defun colorpick-srgb ()
  "Pick color using grim print as srgb; srgb(xxx,xxx,xxx)."
  (interactive)
  (insert (colorpick--type 5)))

(defun colorpick-values ()
  "Pick color using grim print as dec values; (xxx,xxx,xxx)."
  (interactive)
  (insert (colorpick--type 1)))

;;; colorpick.el ends here.