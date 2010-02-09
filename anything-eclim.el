(require 'anything)
(require 'eclim)
(require 'eclim-java)

(defvar anything-c-source-eclim-java-types
  '((name . "Java Types")
    (candidates . anything-eclim-java-type-candidates)
    (action . eclim--visit-declaration)
    (requires-pattern . 3)))

(defun anything-eclim-java-types ()
  (interactive)
  (anything '(anything-c-source-eclim-java-types)))

(defun anything-eclim-java-type-candidates ()
  (mapcar (lambda (x)
	    (cons 
	     (let ((fqcn (nth 2 x)))
	       (if (string-match "\\.\\([^.]+\\)$" fqcn)
		   (concat (match-string 1 fqcn)
			   (propertize
			    (concat
			     " - "
			     (substring fqcn 0 (match-beginning 0)))
			    'face 'shadow))
		 fqcn))
	     x))
	  (eclim/java-search
	   (eclim--project-name)
	   nil nil nil
	   (concat anything-pattern "*") 
	   "type"
	   "declarations"
	   "project"
	   "true")))

(defun anything-eclim-find-display-results (pattern results &optional ignore)
  (when (null base-directory) 
    (setq base-directory (eclim--project-dir)))
  (let ((anything-execute-action-at-once-if-one t)
	(anything-quit-if-no-candidate t))
    (anything
     `((name . ,pattern)
       (candidates . ,(mapcar (lambda (x) (cons (nth 2 x) x)) results))
       (action . eclim--visit-declaration)))))

(provide 'anything-eclim)