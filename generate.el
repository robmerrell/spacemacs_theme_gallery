;; requires s.el to be available and loaded
;; Just evaluate the buffer the files will be generated in ./html
(require 's)
(require 'cl-lib)
(require 'helm-themes)
(require 'htmlize)

;; requires to keep htmlize happy about font faces
(require 'outline)

;; themes that cause errors. These are generally themes with a dark and light version
;; but no default version. For example hemisu has no deftheme, but hemisu-dark and
;; hemisu-light do
(setq bad-themes '("apropospriate" "hemisu" "solarized" "zonokai"))

;; utility functions

(defun get-string-from-file (filePath)
  "Read a file and return the contents as a string"
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defun write-string-to-file (filePath contents)
  (with-temp-buffer
    (insert contents)
    (write-file filePath)))

(defun sub-lists (list1 list2)
  "Subtract list2 from list1"
  (cl-reduce
   (lambda (acc x) (remove x acc)) list2 :initial-value list1))

(defun sorted-helm-themes ()
  "Sort and remove known bad themes from the lists"
  (sub-lists
   (cl-sort (mapcar 'symbol-name (helm-themes--candidates)) 'string-lessp :key 'downcase)
   bad-themes))

(defun mark-and-htmlize-buffer ()
  "Mark the entire current buffer and htmlize it"
  (progn
    (mark-whole-buffer)
    (htmlize-region-for-paste (region-beginning) (region-end))))


;; HTML generation

(defun generate-index-html (tpl themes-html)
  "Create the index.html file"
  (s-format tpl 'elt (list spacemacs-version themes-html)))

(defun generate-theme-div (tpl theme)
  "Generate the div for a single theme"
  (helm-themes--load-theme theme)
  (let* ((buffer-faces (htmlize-faces-in-buffer))
         (face-map (htmlize-make-face-map (adjoin 'default buffer-faces)))
         (style (mapconcat #'identity (htmlize-css-specs (gethash 'default face-map)) " ")))
    (s-format tpl 'elt (list theme style (mark-and-htmlize-buffer)))))

(defun generate-and-join-all-theme-divs (tpl themes)
  "Generate the divs containing the themed content that is embedded inside of the index.html layout"
  (s-join "" (mapcar (lambda (theme) (generate-theme-div tpl theme)) themes)))

(defun generate-theme-gallery ()
  "Generate all of the html needed for the theme gallery and place it's contents in a buffer called theme-gallery"
  (interactive)
  (rainbow-delimiters-mode-disable)
  (let* ((layout-tpl (get-string-from-file "./templates/layout.html"))
         (theme-tpl (get-string-from-file "./templates/theme.html"))
         (themes-html (generate-and-join-all-theme-divs theme-tpl (sorted-helm-themes))))
    (set-buffer (get-buffer-create "theme-gallery"))
    (insert (generate-index-html layout-tpl themes-html)))
  (rainbow-delimiters-mode-enable)
  (switch-to-buffer (current-buffer)))

