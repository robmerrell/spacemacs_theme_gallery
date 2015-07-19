;; requires s.el to be available and loaded
;; Just evaluate the buffer the files will be generated in ./html
(require 's)
(require 'cl-lib)
(require 'helm-themes)

;; requires to keep htmlize happy about font faces
(require 'outline)

;; themes that cause errors. These are generally themes with a dark and light version
;; but no default version. For example hemisu has no deftheme, but hemisu-dark and
;; hemisu-light do
(setq bad-themes '("apropospriate" "hemisu" "solarized" "zonokai"))

(defun get-string-from-file (filePath)
  "Read a file and return the contents as a string"
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defun sub-lists (list1 list2)
  "Subtract list2 from list1"
  (cl-reduce
   (lambda (acc x) (remove x acc)) list2 :initial-value list1))

(defun sorted-helm-themes ()
  "Sort and remove known bad themes from the lists"
  (sub-lists
   (cl-sort (mapcar 'symbol-name (helm-themes--candidates)) 'string-lessp :key 'downcase)
   bad-themes))

(defun generate-theme-files ()
  "Create HTML files for all available themes in the 'html' subdirectory"
  (mapc
   (lambda (theme)
     (helm-themes--load-theme theme)
     (htmlize-file "./sample.exs" (concat "./html/" theme ".html")))
   (sorted-helm-themes)))

(defun generate-index-html (tpl iframes-html)
  "Create the index.html file"
  (s-format tpl 'elt (list spacemacs-version iframes-html)))

(defun generate-iframes-html (tpl themes)
  "Generate the HTML that is embedded inside of the index.html layout"
  (s-join ""
          (mapcar
           (lambda (theme)
             (s-format tpl 'elt (list theme)))
           themes)))

(defun generate-site-files ()
  "The one function to rule them all that generates all of the files in one go. You probably want to call this function."
  (if (file-exists-p "./html")
    (delete-directory "./html" t))
  (mkdir "./html")
  (generate-theme-files)
  (copy-file "./templates/lazyload.min.js" "./html/lazyload.min.js")
  (append-to-file
   (generate-index-html (get-string-from-file "./templates/layout.html")
                        (generate-iframes-html(get-string-from-file "./templates/iframe.html") (sorted-helm-themes)))
   nil "./html/index.html"))

(generate-site-files)
