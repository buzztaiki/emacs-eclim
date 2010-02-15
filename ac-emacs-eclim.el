(require 'eclim)
(require 'eclim-java)
(require 'auto-complete)

(defvar acee-candidates nil)
(defvar acee-start-point nil)
(defvar acee-package-prefix nil)
(defvar acee-use-yasnippet-p nil)
(defvar acee-trigger-commands '(auto-complete ac-trigger-key-command))

(defun acee-candidates ()
  (eclim/java-src-update)
  (let ((ret (eclim/java-complete)))
    (when (and ret (eclim--completion-candidate-doc (car ret)))
      (setq acee-candidates
	    (loop for c in ret
		  collect
		  (cons
		   (acee-candidate-doc c)
		   c)))
      (mapcar 'car acee-candidates))))

(defun acee-prefix ()
  (setq
   acee-start-point
   (and eclim-mode
	(or
	 (save-excursion
	   (ac-prefix-c-dot))
	 (save-excursion
	   ;; TODO: handle method argument
	   (when (and acee-use-yasnippet-p
		      (= (char-before) ?<))
	     (backward-char))
	   (when (and (memq this-command acee-trigger-commands)
		      (< (skip-chars-backward "_a-zA-Z0-9")
			 0)
		      (looking-at "[a-zA-Z]"))
	     (point)))
	 (and (memq this-command acee-trigger-commands)
	      (=
	       (save-excursion
		 (skip-chars-backward " \t" (line-beginning-position))
		 (point))
	       (line-beginning-position))
	      (point))
	 )))
  (setq acee-package-prefix
	  (save-excursion 
	    (and
	     acee-start-point
	     (goto-char acee-start-point)
	     (skip-chars-backward "_a-zA-Z0-9.")
	     (buffer-substring (point) acee-start-point))))
  acee-start-point)

(defun acee-action ()
  (let ((candidate (assoc-default (buffer-substring acee-start-point (point)) acee-candidates)))
    (delete-region acee-start-point (point))
    (unwind-protect
	(when candidate
	  (if acee-use-yasnippet-p
	      (yas/expand-snippet (acee-make-template candidate))
	    (insert (acee-candidate-class candidate))))
      (setq acee-start-point nil)
      (setq acee-package-prefix nil))))

  
(defun acee-candidate-doc (candidate)
  (or (acee-package-substring candidate)
      (eclim--completion-candidate-doc candidate)))

(defun acee-candidate-class (candidate)
  (or (acee-package-substring candidate)
      (eclim--completion-candidate-class candidate)))
      
(defun acee-package-substring (candidate)
  (let ((doc (eclim--completion-candidate-doc candidate)))
    (when (and (string= (eclim--completion-candidate-type candidate) "")
	       (not (string-match "Override" doc))
	       acee-package-prefix
	       (string=
		(substring doc 0 (min (length acee-package-prefix) (length doc)))
		acee-package-prefix))
      (substring doc (length acee-package-prefix)))))

(defun acee-make-template (candidate)
  (let ((type (eclim--completion-candidate-type candidate))
	(str (eclim--completion-candidate-doc candidate)))
    (cond
     ((string= type "f") (acee-make-method-template str))
     ((string= type "c") (acee-make-class-template str))
     (t (acee-candidate-class candidate)))))

(defun acee-make-method-template (str)
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (let ((beg (or (and (search-forward "(" nil t) (match-end 0)) 0))
	  (end (or (and (search-forward ")" nil t) (match-beginning 0)) 0)))
      (delete-region (point) (point-max))
      (unless (= beg end)
	(save-restriction
	  (narrow-to-region beg end)
	  (goto-char (point-max))
	  (insert ",")
	  (goto-char (point-min))
	  (let ((arg-beg (point))
		(i 1))
	    (while (re-search-forward ", *" nil t)
	      (unless (save-match-data
			(save-excursion
			  (let ((p (match-beginning 0)))
			    (and (search-backward "<" arg-beg t)
				 (not (search-forward ">" p t))))))
		(let ((arg (buffer-substring arg-beg (match-beginning 0))))
		  (delete-region arg-beg (point))
		  (insert (format "${%d:%s}, " i arg)))
		(setq i (1+ i))
		(setq arg-beg (point))))
	    (skip-chars-backward ", ")
	    (delete-region (point) (point-max)))))
      (buffer-string))))

(defun acee-make-class-template (str)
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (when (re-search-forward " *-" nil t)
      (delete-region (match-beginning 0) (point-max)))
    (goto-char (point-min))
    (let ((beg (or (and (search-forward "<" nil t) (match-end 0)) 0))
	  (end (or (and (search-forward ">" nil t) (match-beginning 0)) 0)))
      (unless (= beg end)
	(save-restriction
	  (narrow-to-region beg end)
	  (goto-char (point-min))
	  (let ((i 1))
	    (while (re-search-forward "[a-zA-Z]+" nil t)
	      (let ((arg (buffer-substring (match-beginning 0) (match-end 0))))
		(delete-region (match-beginning 0) (match-end 0))
		(insert (format "${%d:%s}" i arg)))
	      (setq i (1+ i))))))
      (buffer-string))))
      

(defvar ac-source-emacs-eclim
  '((candidates . acee-candidates)
    (prefix . acee-prefix)
    (action . acee-action)
    (cache)))

(provide 'ac-emacs-eclim)