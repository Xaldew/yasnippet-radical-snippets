;;; python-mode/.yas-setup.el --- yasnippet setup for python-mode.
;;
;;; Commentary:
;;
;; Setup code for Python based yasnippets.
;;
;;; Code:


(defun yasnippet-radical-snippets--python-split-args (arg-string)
  "Split the python ARG-STRING into ((name, default)..) tuples."
  (mapcar (lambda (x)
             (split-string x "[[:blank:]]*=[[:blank:]]*" t))
          (split-string arg-string "[[:blank:]]*,[[:blank:]]*" t)))


(defun yasnippet-radical-snippets--python-args-to-reST-docstring (text &optional make-fields)
  "Return a reST docstring format for the python arguments in TEXT.

Optional argument MAKE-FIELDS will create yasnippet compatible
field that the can be jumped to upon further expansion."
  (let* ((indent (concat "\n" (make-string (current-column) 32)))
         (args (yasnippet-radical-snippets--python-split-args text))
	 (nr 0)
         (formatted-args
	  (mapconcat
	   (lambda (x)
	     (concat ":param " (nth 0 x) ":"
		     (if make-fields (format " ${%d:arg%d}" (cl-incf nr) nr))
		     (if (nth 1 x) (concat " \(default " (nth 1 x) "\)"))))
	   args
	   indent)))
    (unless (string= formatted-args "")
      (concat
       indent
       (mapconcat 'identity
		  (list ".. Keyword Arguments:" formatted-args)
		  indent)
       indent))))


(defun yasnippet-radical-snippets--python-types-to-reST-docstring (text &optional make-fields)
  "Return a ReST docstring format for the Python arguments in TEXT.

Optional argument MAKE-FIELDS will create yasnippet compatible
field that the can be jumped to upon further expansion."
  (let* ((indent (concat "\n" (make-string (current-column) 32)))
         (args (yasnippet-radical-snippets--python-split-args text))
	 (i 0)
	 (nr (length args))
	 (formatted-types
	  (mapconcat (lambda (x)
		       (concat ":type " (nth 0 x) ":"
			       (if make-fields
				   (format " ${%d:type%d}"
					   (cl-incf nr) (cl-incf i)))))
		     args indent)))
    (unless (string= formatted-types "")
      (concat
       (mapconcat 'identity (list  ".. Types:" formatted-types) indent)
       indent))))


(defun yasnippet-radical-snippets--python-args-to-google-docstring (text &optional make-fields)
  "Return a Google docstring for the Python arguments in TEXT.

Optional argument MAKE-FIELDS will create yasnippet compatible
field that the can be jumped to upon further expansion."
  (let* ((indent (concat "\n" (make-string (current-column) 32)))
         (args (yasnippet-radical-snippets--python-split-args text))
    	 (nr 0)
         (formatted-args
    	  (mapconcat
    	   (lambda (x)
    	     (concat "   " (nth 0 x)
    		     (if make-fields (format " ${%d:arg%d}" (cl-incf nr) nr))
    		     (if (nth 1 x) (concat " \(default " (nth 1 x) "\)"))))
    	   args
    	   indent)))
    (unless (string= formatted-args "")
      (concat
       (mapconcat 'identity
    		  (list "" "Args:" formatted-args)
    		  indent)
       "\n"))))


(provide 'python-mode/.yas-setup.el)

;;; .yas-setup.el ends here
