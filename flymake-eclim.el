(require 'eclim)

(defvar flymake-eclim-program "flymake-eclim")
(defvar flymake-eclim-sleep-time 1)
(defvar flymake-eclim-file-name-mask
  '("\\.java\\'" flymake-eclim-init))
(defvar flymake-eclim-err-line-pattern
  '("\\(.+\\)|\\(.+\\)|\\(.+\\)|\\(.+\\)"
    1 2 3 4))

(defun flymake-eclim-init ()
  (let ((project (eclim--project-name)))
    (when project
      (list flymake-eclim-program
	    (list
	     (expand-file-name eclim-executable)
	     project
	     (eclim--project-current-file)
	     (number-to-string flymake-eclim-sleep-time))))))

(defun flymake-eclim-setup ()
  (set (make-local-variable 'flymake-start-syntax-check-on-newline) nil)
  (set (make-local-variable 'flymake-allowed-file-name-masks)
       (list flymake-eclim-file-name-mask))
  (set (make-local-variable 'flymake-err-line-patterns)
       (list flymake-eclim-err-line-pattern))
  (flymake-mode 1)
  (when (timerp flymake-timer)
    (cancel-timer flymake-timer)
    (setq flymake-timer nil)))

(provide 'flymake-eclim)