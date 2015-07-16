;; requires s.el to be available and loaded
(require 's)
(require 'cl-lib)

(defun get-string-from-file (filePath)
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defun sorted-helm-themes ()
  (cl-sort (mapcar 'symbol-name (helm-themes--candidates)) 'string-lessp :key 'downcase))

(defun generate-theme-files ()
  (mapc
   (lambda (theme)
     (helm-themes--load-theme theme)
     (htmlize-file "./sample.exs" (concat "./html/" theme ".html")))
   (sorted-helm-themes)))

(defun generate-index-html (tpl iframes-html)
  (s-format tpl 'elt (list spacemacs-version iframes-html)))

(defun generate-iframes-html (tpl themes)
  (s-join ""
          (mapcar
           (lambda (theme)
             (s-format tpl 'elt (list theme)))
           themes)))

(defun generate-site-files ()
  (delete-directory "./html")
  (generate-theme-files)
  (append-to-file
   (generate-index-html (get-string-from-file "./templates/layout.html")
                        (generate-iframes-html(get-string-from-file "./templates/iframe.html") (sorted-helm-themes)))
   nil "./html/index.html"))

(generate-site-files)
