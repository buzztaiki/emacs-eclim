(require 'eclim)
(require 'eclim-project)
(require 'flymake)

(defvar flymake-eclim-file-name-mask
  '("\\.java\\'" flymake-eclim-init))
(defvar flymake-eclim-err-line-pattern
  ;; file|pos|msg|type
  "\\(.+\\)|\\([0-9]+\\)\\(?: col [0-9]+\\)?|\\(.+\\)|\\(.+\\)")
    

(defun flymake-eclim-init ()
  (when (and eclim-mode flymake-eclim-mode)
    (let ((project (eclim--project-name)))
      (when project
	(list (expand-file-name eclim-executable)
	      (list
	       "-command" "java_src_update"
	       "-p" project
	       "-f" (eclim--project-current-file)
	       "-v"))))))

(define-minor-mode flymake-eclim-mode
  "flymake for eclim."
  :global nil
  :group 'eclim
  ;; enable advice at first.
  (ad-enable-advice 'flymake-parse-line 'around 'flymake-eclim)
  (ad-activate 'flymake-parse-line)
  (if flymake-eclim-mode
      (progn
	(set (make-local-variable 'flymake-allowed-file-name-masks)
	     (list flymake-eclim-file-name-mask))
	(flymake-mode 1)
	;; don't run flymake after change.
	(remove-hook 'after-change-functions 'flymake-after-change-function t))
    (flymake-mode -1)))
     
;;; hack
(defun flymake-eclim-parse-line (line)
  (when (string-match flymake-eclim-err-line-pattern line)
    (flymake-ler-make-ler
     (match-string 1 line)
     (string-to-number (match-string 2 line))
     (match-string 4 line)
     (match-string 3 line))))


(defadvice flymake-parse-line (around flymake-eclim
				      (line)
				      disable)
  (if flymake-eclim-mode
      (setq ad-return-value (flymake-eclim-parse-line line))
    ad-do-it))

(provide 'flymake-eclim)
