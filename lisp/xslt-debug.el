;;
;;  Author: Ovidiu Predescu <ovidiu@cup.hp.com>
;;  Date: March 26, 2001
;;

;;; User defaults

(defvar xslt-debug-mode-line-string " XSLTd"
  "*String to appear in the modeline when the XSLT debug mode is active.")

;;; End of user customizations

(defvar xslt-debug-breakpoints (make-hashtable 10 'equal)
  "Hash table containing the currently defined breakpoints.")

(defvar xslt-debug-mode-map (make-sparse-keymap)
  "Keyboard bindings for the XSLT debug mode.")

(defvar xslt-debug-debugger-started nil
  "Indicates whether the XSLT debugger process is running.")

(defvar xslt-debug-mode nil)

(make-variable-buffer-local 'xslt-debug-mode)


(defun xslt-debug-mode (&optional arg)
  "*Setup a buffer in the XSLT debugging mode.
This essentially makes the buffer read-only and binds various keys to
different actions for faster operations."
  (interactive "P")
  ;; Enter the XSLT-debug mode only if we've not done so before
  (if xslt-debug-mode
      nil
    (setq xslt-debug-mode t)
    (toggle-read-only 1)
    ;; Set the same keybindings as in the editing mode
    (define-key xslt-debug-mode-map
      xslt-debug-set-breakpoint 'xslt-debug-set-breakpoint)
    (define-key xslt-debug-mode-map
      xslt-debug-delete-breakpoint 'xslt-debug-delete-breakpoint)

    (define-key xslt-debug-mode-map "q" 'xslt-debug-quit)
    (define-key xslt-debug-mode-map "b" 'xslt-debug-set-breakpoint)
    (define-key xslt-debug-mode-map "d" 'xslt-debug-delete-breakpoint)))

(defun xslt-debug-quit ()
  "*Quit the debugger and exit from the XSLT-debug mode."
  (interactive)
  (setq xslt-debug-mode nil)
  (toggle-read-only 0))

(defun xslt-debug-set-breakpoint ()
  "*Set a breakpoint at line in the current buffer."
  (interactive)
  (let* ((filename (buffer-file-name))
	 (line (save-excursion (progn (end-of-line) (count-lines 1 (point)))))
	 (breakpoint (gethash '(filename . line) xslt-debug-breakpoints)))
    (if (eq breakpoint nil)
	(progn
	  (puthash '(filename . line) '(filename . line)
		   xslt-debug-breakpoints)
	  (message (format "Set breakpoint in %s at %s." filename line)))
      (message (format "Breakpoint already set in %s at %s" filename line)))))

(defun xslt-debug-delete-breakpoint ()
  "*Remove the breakpoint at current line in the selected buffer."
  (interactive)
  (let* ((filename (buffer-file-name))
	 (line (save-excursion (progn (end-of-line) (count-lines 1 (point)))))
	 (breakpoint (gethash '(filename . line) xslt-debug-breakpoints)))
    (if (not (eq breakpoint nil))
	(progn
	  (remhash '(filename . line) xslt-debug-breakpoints)
	  (message
	   (format "Removed breakpoint in %s at %s." filename line)))
      (message (format "No breakpoint in %s at %s" filename line)))))

(defun xslt-debug-is-breakpoint ()
  "*Checks whether there's a breakpoint setup in buffer at line."
  (let* ((filename (buffer-file-name))
	 (line (save-excursion (progn (end-of-line) (count-lines 1 (point)))))
	 (breakpoint (gethash '(filename . line) xslt-debug-breakpoints)))
    (not (eq breakpoint nil))))

(if (fboundp 'add-minor-mode)
    (add-minor-mode 'xslt-debug-mode
		    xslt-debug-mode-line-string
		    xslt-debug-mode-map
		    nil
		    'xslt-debug-mode)
  (or (assoc 'xslt-debug-mode minor-mode-alist)
      (setq minor-mode-alist
	    (cons '(xslt-debug-mode xslt-debug-mode-line-string)
		  minor-mode-alist)))

  (or (assoc 'xslt-debug-mode minor-mode-map-alist)
      (setq minor-mode-map-alist
	    (cons (cons 'xslt-debug-mode xslt-debug-mode-map)
		  minor-mode-map-alist))))


(provide 'xslt-debug)
