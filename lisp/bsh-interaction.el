;;; bsh-interact.el -- Interact with a BSH process
;;
;; Author: Ovidiu Predescu <ovidiu@cup.hp.com>
;; Created: March 15, 2001
;; Time-stamp: <March 16, 2001 00:47:13  ovidiu>
;;

(require 'eieio)

(defclass bsh ()
  ;; Class variables
  ((process-to-bsh-object :allocation :class
			  :initform (lambda () (make-weak-list 'assoc))
			  :documentation "A weak hash table to keep 
track of which bsh instance is associated with a particular Emacs
process.")

  ;; Instance variables
  (process-name :type string
		 :initarg :process-name
		 :initform ""
		 :documentation "The name of the process.")
   (timeout :type number
	    :initarg :timeout
	    :initform 20
	    :documentation "Timeout to wait for a reply from BSH.")
   (classname :type string
	      :initarg :classname
	      :initform "bsh.Interpreter"
	      :documentation "The name of the Java class to be invoked.")
   (bsh-args :type string
	     :initarg :bsh-args
	     :initform ""
	     :documentation"Additional arguments passed to BSH at startup.")
   (reply :type string
	  :initform ""
	  :documentation "Temporarily holds the reply from BSH.")
   (process :initform nil
	    :documentation "The process object"))
  "Class to manage the interaction with a Java BeanShell process.")

(defmethod bsh-start (o)
  "Start the Java BeanShell as a separate process."
  (if (or (null (oref o process))
	  (not (eq (process-status (oref o process)) 'run)))
      (let* ((process-name (oref o process-name))
	     (buffer-name (concat "*" process-name "*")))
	(setq process (start-process process-name buffer-name
				     "java" (oref o classname)
				     (oref o bsh-args)))
	(puthash process o (oref o process-to-bsh-object))
	(set-process-filter bsh-filter))))

(defmethod eval (obj expr)
  (bsh-start obj)
  (process-send-string (oref obj process) expr))

(defun bsh-filter (proc result)
  (let ((end-of-result (string-match ".*bsh % " result))
	(bsh (gethash proc (oref bsh process-to-bsh-object))))
    ;; Check for case
    ;;   %bsh\n...eval output...%bsh\n
    ;; This can happen because the beanshell outputs two or more
    ;; prompts after evaluating some expressions.
    ;; Thanks to Stephane Nicolas.
    ;; (if (eq end-of-result 0)
    ;; (accept-process-output process 0 5))
    (if end-of-result
	(setq reply (concat reply (substring result 0 end-of-result)))
      (setq reply (concat reply result))
      (accept-process-output proc timeout)))
  ;; Insert the data into the buffer
  (let ((old-buffer (current-buffer)))
    (unwind-protect
	(let ((marker (process-mark proc))
	      moving)
	  (set-buffer (process-buffer proc))
	  (setq moving (= (point) marker))
	  (save-excursion
	    ;; Insert the text, moving the process-marker
	    (goto-char marker)
	    (insert result)
	    (set-marker (process-mark proc)))
	  (if moving (goto-char (process-mark proc))))
      (set-buffer old-buffer))))

(provide 'bsh-interaction)

(setq bsh-object (make-instance bsh :process-name "BSH"))

(bsh-start bsh-object)
(oref bsh-object timeout)
