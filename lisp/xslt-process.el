;;;; xslt-process.el -- Invoke an XSLT processor on an Emacs buffer

;; Package: xslt-process
;; Author: Ovidiu Predescu <ovidiu@cup.hp.com>
;; Created: December 2, 2000
;; Time-stamp: <March 2, 2001 20:55:28  ovidiu>
;; Keywords: XML, XSLT
;; URL: http://www.geocities.com/SiliconValley/Monitor/7464/
;; Compatibility: XEmacs 21.1, Emacs 20.4

;; This file is not part of GNU Emacs

;; Copyright (C) 2000, 2001 Ovidiu Predescu

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Comentary:

;; To use this package, put the lisp/ directory from this package in
;; your Emacs load-path and do:
;;
;; (autoload 'xslt-process-mode "xslt-process" "Run XSLT processor on buffer" t)
;;
;; Then, while being in an XML buffer, use C-cx to invoke the XSLT
;; processor of your choice. The result will be displayed in another
;; buffer.

(require 'jde)
(require 'cl)

;;; User defaults

(defgroup xslt-process nil
  "Run an XSLT processor on an Emacs buffer."
  :group 'tools)

(defcustom xslt-process-default-processor (list 'Saxon)
  "*The default XSLT processor to be applied to an XML document."
  :group 'xslt-process
  :type '(list
	  (radio-button-choice
	   (const :tag "Saxon" Saxon)
	   (const :tag "Xalan 1.x" Xalan1)
	   (const :tag "Generic TrAX processor (Saxon 6.1 and greater, Xalan2 etc.)" TrAX)
	   (const :tag "Cocoon 1.x" Cocoon1))))

(defcustom xslt-process-cocoon1-properties-file ""
  "*The location of the Cocoon 1.x properties file."
  :group 'xslt-process
  :type '(file :must-match t :tag "Properties file"))

(defcustom xslt-process-jvm-arguments nil
  "*Additional arguments to be passed to the JVM.
Use this option to pass additional arguments to the JVM that might be
needed for the XSLT processor to function correctly."
  :group 'xslt-process
  :type '(repeat (string :tag "Argument")))

;;;###autoload
(defcustom xslt-process-additional-classpath nil
  "*Additional Java classpath to be passed when invoking Bean Shell.
Note that modifying this won't have any effect until you restart the
Bean Shell. You can do this by killing the *bsh* buffer."
  :group 'xslt-process
  :type '(repeat (file :must-match t :tag "Path")))

;;;###autoload
(defcustom xslt-process-key-binding "\C-c\C-x\C-v"
  "*Keybinding for invoking the XSLT processor.
To enter a normal key, enter its corresponding character. To enter a
key with a modifier, either type C-q followed by the desired modified
keystroke, e.g. C-q C-c to enter Control c. To enter a function key,
use the [f1], [f2] etc. notation."
  :group 'xslt-process
  :type '(string :tag "Key"))

(defcustom xslt-process-toggle-debug-mode "\C-c\C-x\C-d"
  "*Keybinding for toggling the debug mode."
  :group 'xslt-process
  :type '(string :tag "Key"))

(defcustom xslt-process-set-breakpoint "b"
  "*Keybinding for setting up a breakpoint at line in the current buffer.
The buffer has to be in the debug mode for this key to work."
  :group 'xslt-process
  :type '(string :tag "Key"))

(defcustom xslt-process-delete-breakpoint "d"
  "*Keybinding for deleting the breakpoint at line in the current buffer.
The buffer has to be in the debug mode for this key to work"
  :group 'xslt-process
  :type '(string :tag "Key"))

(defcustom xslt-process-enable/disable-breakpoint "e"
  "*Keybinding for enabling or disabling the breakpoint at line in the
current buffer. The buffer has to be in the debug mode for this key
to work"
  :group 'xslt-process
  :type '(string :tag "Key"))

(defcustom xslt-process-quit-debug "q"
  "*Keybinding for exiting from the debug mode. The buffer has to be
in the debug mode for this key to work."
  :group 'xslt-process
  :type '(string :tag "Key"))

;;;###autoload
(defcustom xslt-process-mode-line-string " XSLT"
  "*String displayed in the modeline when the xslt-process minor
mode is active. Set this to nil if you don't want a modeline
indicator."
  :group 'xslt-process
  :type 'string)

(defcustom xslt-process-debug-mode-line-string " XSLTd"
  "*String to appear in the modeline when the XSLT debug mode is active."
  :group 'xslt-process
  :type 'string)

;;; End of user customizations

;;; Minor mode definitions

;;;###autoload
(defvar xslt-process-mode nil
  "Indicates whether the current buffer is in the XSLT-process minor mode.")
(make-variable-buffer-local 'xslt-process-mode)

;;;###autoload
(defvar xslt-process-debug-mode nil
  "Indicates whether the current buffer is in debug mode.")
(make-variable-buffer-local 'xslt-process-debug-mode)

;;;###autoload
(defvar xslt-process-mode-map (make-sparse-keymap)
  "Keyboard bindings in normal XSLT-process mode enabled buffers.")

(defvar xslt-process-debug-mode-map (make-sparse-keymap)
  "Keyboard bindings for the XSLT debug mode.")

(defvar xslt-process-breakpoints (make-hashtable 10 'equal)
  "Hash table containing the currently defined breakpoints.")

;;;###autoload
(defun xslt-process-mode (&optional arg)
  "Minor mode to invoke an XSLT processor on the current buffer.

This mode spawns off a Java Bean Shell process in the background to
run an XSLT processor of your choice. This minor mode makes use of
Emacs-Lisp functionality defined in JDE, the Java Development
Environment for Emacs.

With no argument, this command toggles the xslt-process mode. With a
prefix argument ARG, turn xslt-process minor mode on iff ARG is
positive.

Bindings:
\\[xslt-process-invoke]: Invoke the XSLT processor on the current buffer.

Hooks:
xslt-process-hook is run after the xslt-process minor mode is entered.

For more information please check:

xslt-process:    http://www.geocities.com/SiliconValley/Monitor/7464/
Emacs JDE:       http://sunsite.dk/jde/
Java Bean Shell: http://www.beanshell.org/
"
  (interactive "P")
  (add-submenu
   nil
   '("XSLT"
     ["Run" xslt-process-invoke :active t]
     ["--:singleLine" nil]
     ["Toggle debug mode" xslt-process-toggle-debug-mode :active t]
     ["Set breakpoint" xslt-process-set-breakpoint
      :active (and xslt-process-debug-mode (xslt-process-is-breakpoint))]
     ["Delete breakpoint" xslt-process-delete-breakpoint
      :active (and xslt-process-debug-mode (not (xslt-process-is-breakpoint)))]
     ["Disable breakpoint" xslt-process-enable/disable-breakpoint
      :active (and xslt-process-debug-mode (xslt-process-is-breakpoint))]
     ["--:singleLine" nil]
     ["Start Debugger" xslt-process-mode :active t]
     ["Step" xslt-process-do-next :active nil]
     ["Next" xslt-process-do-next :active nil]
     ["Finish" xslt-process-do-next :active nil]
     ["Continue" xslt-process-do-next :active nil]))
  (setq xslt-process-mode
	(if (null arg) (not xslt-process-mode)
	  (> (prefix-numeric-value arg) 0)))
  (define-key xslt-process-mode-map
    xslt-process-key-binding 'xslt-process-invoke)
					; Force modeline to redisplay
  (set-buffer-modified-p (buffer-modified-p)))

(defun xslt-process-toggle-debug-mode (&optional arg)
  "*Setup a buffer in the XSLT debugging mode.
This essentially makes the buffer read-only and binds various keys to
different actions for faster operations."
  (interactive "P")
  ;; Enter the xslt-process mode only if we've not done so before
  (if xslt-process-mode
      nil
    (setq xslt-process-mode t)
    (toggle-read-only 1)
    ;; Set the same keybindings as in the editing mode
    (define-key xslt-process-debug-mode-map
      xslt-process-set-breakpoint 'xslt-process-set-breakpoint)
    (define-key xslt-process-debug-mode-map
      xslt-process-delete-breakpoint 'xslt-process-delete-breakpoint)

    (define-key xslt-process-debug-mode-map
      xslt-process-quit-debug 'xslt-process-quit-debug)
    (define-key xslt-process-debug-mode-map
      xslt-process-set-breakpoint 'xslt-process-set-breakpoint)
    (define-key xslt-process-debug-mode-map
      xslt-process-delete-breakpoint 'xslt-process-delete-breakpoint)
    (define-key xslt-process-debug-mode-map
      xslt-process-enable/disable-breakpoint
      'xslt-process-enable/disable-breakpoint)))

(put 'Saxon 'additional-params 'xslt-saxon-additional-params)
(put 'Xalan1 'additional-params 'xslt-xalan1-additional-params)
(put 'TrAX 'additional-params 'xslt-trax-additional-params)
(put 'Cocoon1 'additional-params 'xslt-cocoon1-additional-params)

(defun xslt-saxon-additional-params ())
(defun xslt-xalan1-additional-params ())
(defun xslt-trax-additional-params ())

(defun xslt-cocoon1-additional-params ()
  (if (or (null xslt-process-cocoon1-properties-file)
	  (equal xslt-process-cocoon1-properties-file ""))
      (error "No Cocoon properties file specified."))
  (bsh-eval (concat "xslt.Cocoon1.setPropertyFilename(\""
		    xslt-process-cocoon1-properties-file "\");"))
  (setq cocoon-user-agent
	(if (and
	     (local-variable-p 'user-agent (current-buffer))
	     (boundp 'user-agent))
	    (if (stringp user-agent)
		user-agent
	      (symbol-name user-agent))
	  nil))
  (bsh-eval (concat "xslt.Cocoon1.setUserAgent(\""
		    cocoon-user-agent "\");"))
  (makunbound 'user-agent))

(defun xslt-process-find-xslt-directory ()
  "Return the path to the xslt-process directory."
  (file-truename
   (concat (file-name-directory (locate-library "xslt-process")) "../")))

(defun xslt-process-invoke ()
  "This is the main function which invokes the XSLT processor of your
choice on the current buffer."
  (interactive)
  (let* ((temp-directory
	  (or (if (fboundp 'temp-directory) (temp-directory))
	      (if (boundp 'temporary-file-directory) temporary-file-directory)))
	 (classpath
	  (if (boundp 'jde-global-classpath)
	      jde-global-classpath
	    nil))
	 (classpath-env (if (getenv "CLASSPATH")
			    (split-string (getenv "CLASSPATH")
					  jde-classpath-separator)
			  nil))
	 (out-buffer (get-buffer-create "*xslt output*"))
	 (msg-buffer (get-buffer-create "*xslt messages*"))
	 (filename (if (buffer-file-name)
		       (expand-file-name (buffer-file-name))
		     (error "No filename associated with this buffer.")))
	 (xslt-jar (concat
		    (xslt-process-find-xslt-directory) "java/xslt.jar"))
	 (tmpfile (make-temp-name (concat temp-directory "/xsltout")))
	 ; Set the name of the XSLT processor. This is either specified
	 ; in the local variables of the file or is the default one.
	 (xslt-processor
	  (progn
	    ; Force evaluation of local variables
	    (hack-local-variables t)
	    (or
	     (if (and
		  (local-variable-p 'processor (current-buffer))
		  (boundp 'processor))
		 (if (stringp processor)
		     processor
		   (symbol-name processor)))
	     (symbol-name (car xslt-process-default-processor))))))
    (save-excursion
      ; Reset any local variables in the source buffer so the next
      ; time we execute we correctly pick up the default processor
      ; even if the user decides to remove the local variable
      (makunbound 'processor)
      ; Prepare to invoke the Java method to process the XML document
      (setq jde-global-classpath
	    (mapcar 'expand-file-name
		    (union (append jde-global-classpath (list xslt-jar))
			   (union xslt-process-additional-classpath
				  classpath-env))))
      ; Append the additional arguments to the arguments passed to bsh
      (setq bsh-vm-args (union xslt-process-jvm-arguments bsh-vm-args))
      ; Setup additional arguments to the processor
      (setq func (get (intern-soft xslt-processor) 'additional-params))
      (if (not (null func)) (funcall func))
      ; Prepare the buffers
      (save-some-buffers)
      (set-buffer msg-buffer)
      (erase-buffer)
      (set-buffer out-buffer)
      (erase-buffer)
      ; Invoke the processor, displaying the result in a buffer and
      ; any error messages in an additional buffer
      (condition-case nil
	  (progn
	    (setq messages (bsh-eval
			    (concat "xslt." xslt-processor ".invoke(\""
				    filename "\", \"" tmpfile
				    "\");")))
	    (setq jde-global-classpath classpath)
	    (if (file-exists-p tmpfile)
		(progn
		  (set-buffer out-buffer)
		  (insert-file-contents tmpfile)
		  (delete-file tmpfile)
		  (display-buffer out-buffer)
		  (if (not (string= messages ""))
		      (xslt-process-display-messages messages
						     msg-buffer out-buffer))
		  (message "Done invoking %s." xslt-processor))
	      (message (concat "Cannot process "
			       (file-name-nondirectory filename) "."))
	      (xslt-process-display-messages messages msg-buffer out-buffer)))
	(error (progn
		 (message
		  (concat "Could not process file, most probably "
			  xslt-processor
			  " could not be found!"))
		 (setq jde-global-classpath classpath)))))))

(defun xslt-process-display-messages (messages msg-buffer out-buffer)
  (set-buffer msg-buffer)
  (insert messages)
  (let ((msg-window (get-buffer-window msg-buffer))
	(out-window (get-buffer-window out-buffer)))
    (if (not msg-window)
	(split-window out-window))
    (display-buffer msg-buffer)))  

(defun xslt-process-set-breakpoint ()
  "*Set a breakpoint at line in the current buffer."
  (interactive)
  (let* ((filename (buffer-file-name))
	 (line (save-excursion (progn (end-of-line) (count-lines 1 (point)))))
	 (breakpoint (gethash '(filename . line) xslt-process-breakpoints)))
    (if (eq breakpoint nil)
	(progn
	  (puthash '(filename . line) '(filename . line)
		   xslt-process-breakpoints)
	  (message (format "Set breakpoint in %s at %s." filename line)))
      (message (format "Breakpoint already set in %s at %s" filename line)))))

(defun xslt-process-delete-breakpoint ()
  "*Remove the breakpoint at current line in the selected buffer."
  (interactive)
  (let* ((filename (buffer-file-name))
	 (line (save-excursion (progn (end-of-line) (count-lines 1 (point)))))
	 (breakpoint (gethash '(filename . line) xslt-process-breakpoints)))
    (if (not (eq breakpoint nil))
	(progn
	  (remhash '(filename . line) xslt-process-breakpoints)
	  (message
	   (format "Removed breakpoint in %s at %s." filename line)))
      (message (format "No breakpoint in %s at %s" filename line)))))

(defun xslt-process-enable/disable-breakpoint ()
  "*Enable or disable the breakpoint at the current line in buffer, depending
on its state."
  (interactive))

(defun xslt-process-is-breakpoint ()
  "*Checks whether there's a breakpoint setup in buffer at line."
  (let* ((filename (buffer-file-name))
	 (line (save-excursion (progn (end-of-line) (count-lines 1 (point)))))
	 (breakpoint (gethash '(filename . line) xslt-process-breakpoints)))
    (not (eq breakpoint nil))))

(defun xslt-process-quit-debug ()
  "*Quit the debugger and exit from the xslt-process mode."
  (interactive)
  (xslt-process-toggle-debug-mode 0))

;;;###autoload
(if (fboundp 'add-minor-mode)
    (add-minor-mode 'xslt-process-mode
		    xslt-process-mode-line-string
		    xslt-process-mode-map
		    nil
		    'xslt-process-mode)
  (or (assoc 'xslt-process-mode minor-mode-alist)
      (setq minor-mode-alist
	    (cons '(xslt-process-mode xslt-process-mode-line-string)
		  minor-mode-alist)))

  (or (assoc 'xslt-process-mode minor-mode-map-alist)
      (setq minor-mode-map-alist
	    (cons (cons 'xslt-process-mode xslt-process-mode-map)
		  minor-mode-map-alist))))

(provide 'xslt-process)
