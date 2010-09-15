(require 'anything)
(require 'eclim)
(require 'eclim-java)

(defvar anything-c-source-eclim-java-types
  '((name . "Java Types")
    (candidates . anything-eclim-java-type-candidates)
    (candidate-transformer . anything-eclim-candidate-transformer)
    (action . eclim--visit-declaration)
    (requires-pattern . 3)))

(defun anything-eclim-java-types ()
  (interactive)
  (anything '(anything-c-source-eclim-java-types)))

(defun anything-eclim-java-type-candidates ()
  (start-process
   "anything-eclim-java-type-candidates" nil
   (expand-file-name eclim-executable)
   "-command" "java_search"
   "-n" (with-current-buffer anything-current-buffer
	  (eclim--project-name))
   "-t" "type"
   "-x" "declarations"
   "-s" "project"
   "-i"
   "-p" (concat anything-pattern "*")))

(defun anything-eclim-candidate-transformer (candidates)
  (loop for x in candidates
	collect
	(let* ((resp (split-string x "|"))
	       (fqcn (nth 2 resp)))
	  (cons
	   (if (and fqcn (string-match "^\\(.+\\)\\.\\([^.]+\\)$" fqcn))
	       (format "%s - %s"
		       (match-string 2 fqcn)
		       (propertize (match-string 1 fqcn)
				   'face 'shadow))
	     fqcn)
	   resp))))

(defun anything-eclim-find-display-results (pattern results)
  (when (null base-directory) 
    (setq base-directory (eclim--project-dir)))
  (let ((anything-execute-action-at-once-if-one t)
	(anything-quit-if-no-candidate t))
    (anything
     `((name . ,pattern)
       (candidates . ,(mapcar (lambda (x) (cons (nth 2 x) x)) results))
       (action . eclim--visit-declaration)))))

(provide 'anything-eclim)