(require 'eclim)
(require 'eclim-java)
(require 'auto-complete)

(defvar acee-candidates nil)
(defvar acee-start-point nil)
(defvar acee-use-yasnippet-p (require 'yasnippet nil t))

(defun acee-candidates ()
  (eclim/java-src-update)
  (let ((ret (eclim/java-complete)))
    (when ret
      (setq acee-candidates
	    (loop for c in ret
		  collect (cons (eclim--completion-candidate-doc c) c)))
      (mapcar 'car acee-candidates))))

(defun acee-prefix ()
  (setq
   acee-start-point
   (and eclim-mode
	(or
	 (save-excursion
	   (when (save-excursion (re-search-backward "\\<package\\|import\\>" (line-beginning-position) t))
	     (and (skip-chars-backward "_a-zA-Z0-9.")
		  (point))))
	 (save-excursion
	   (ac-prefix-c-dot))
	 (save-excursion
	   ;; TODO: handle method argument
	   (when (and acee-use-yasnippet-p
		      (= (char-before) ?<))
	     (backward-char))
	   (when (and (< (skip-chars-backward "_a-zA-Z0-9")
			 0)
		      (looking-at "[a-zA-Z]"))
	     (point)))
	 (and (not ac-auto-start)
	      (= (char-before) ?\ )
	      (point))))))

(defun acee-action ()
  (let ((candidate (assoc-default (buffer-substring acee-start-point (point)) acee-candidates)))
    (delete-region acee-start-point (point))
    (setq acee-start-point nil)
    (when candidate
      (if acee-use-yasnippet-p
	  (yas/expand-snippet (acee-make-template candidate))
	(insert (eclim--completion-candidate-class candidate))))))
  
(defun acee-make-template (candidate)
  (let ((type (eclim--completion-candidate-type candidate))
	(str (eclim--completion-candidate-doc candidate)))
    (cond
     ((string= type "f") (acee-make-method-template str))
     ((string= type "c") (acee-make-class-template str))
     (t (eclim--completion-candidate-class candidate)))))

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