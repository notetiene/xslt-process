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
;; Then, while being in an XML buffer, use the XSLT menu to either:
;;
;; - run an XSLT processor on the buffer and display the results in a
;; different one
;;
;; - run an XSLT processor in debug mode, so you can view the XSLT
;; processing as it happens
;;

(require 'jde)
(require 'cl)

;; From "custom" web page at http://www.dina.dk/~abraham/custom/
(eval-and-compile
  (condition-case ()
      (require 'custom)
    (error nil))
  (if (and (featurep 'custom) (fboundp 'custom-declare-variable))
      nil ;; We've got what we needed
    ;; We have the old custom-library, hack around it!
    (defmacro defgroup (&rest args)
      nil)
    (defmacro defface (var values doc &rest args)
      (` (progn
	   (defvar (, var) (quote (, var)))
	   ;; To make colors for your faces you need to set your .Xdefaults
	   ;; or set them up ahead of time in your .emacs file.
	   (make-face (, var))
	   )))
    (defmacro defcustom (var value doc &rest args)
      (` (defvar (, var) (, value) (, doc))))))

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

(defcustom xslt-process-additional-classpath nil
  "*Additional Java classpath to be passed when invoking Bean Shell.
Note that modifying this won't have any effect until you restart the
Bean Shell. You can do this by killing the *bsh* buffer."
  :group 'xslt-process
  :type '(repeat (file :must-match t :tag "Path")))

(defcustom xslt-process-key-binding "\C-c\C-xv"
  "*Keybinding for invoking the XSLT processor.
To enter a normal key, enter its corresponding character. To enter a
key with a modifier, either type C-q followed by the desired modified
keystroke, e.g. C-q C-c to enter Control c. To enter a function key,
use the [f1], [f2] etc. notation."
  :group 'xslt-process
  :type '(string :tag "Key"))

(defcustom xslt-process-toggle-debug-mode "\C-c\C-xd"
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

(defface xslt-process-enabled-breakpoint-face
  '((((class color) (background light))
     (:foreground "purple" :background "salmon")))
  "*Face used to highlight enabled breakpoints."
  :group 'font-lock-highlighting-faces)

(defface xslt-process-disabled-breakpoint-face
  '((((class color) (background light))
     (:foreground "purple" :background "wheat3")))
  "*Face used to highlight disabled breakpoints."
  :group 'font-lock-highlighting-faces)

(defface xslt-process-current-line-face
  '((((class color) (background light))
     (:foreground "black" :background "orange")))
  "*Face used to highlight disabled breakpoints."
  :group 'font-lock-highlighting-faces)

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


;; Definitions used further
;;;###autoload
(defvar xslt-process-mode nil
  "Indicates whether the current buffer is in the XSLT-process minor mode.")
(make-variable-buffer-local 'xslt-process-mode)

;;;###autoload
(defvar xslt-process-debug-mode nil
  "Indicates whether the current buffer is in debug mode.")
(make-variable-buffer-local 'xslt-process-debug-mode)

(defvar xslt-process-mode-line xslt-process-mode-line-string
  "The string to be displayed in the modeline as the minor mode
indicator.  Initially this is the same with the mode-line-string, but
it can change to debug-mode-line-string, if the debug mode is
entered.")

;;;###autoload
(defvar xslt-process-mode-map (make-sparse-keymap)
  "Keyboard bindings in normal XSLT-process mode enabled buffers.")

(defvar xslt-process-debug-mode-map (make-sparse-keymap)
  "Keyboard bindings for the XSLT debug mode.")

(defvar xslt-process-breakpoints (make-hashtable 10 'equal)
  "Hash table containing the currently defined breakpoints.")

(defvar xslt-process-comint-process-name "xslt"
  "The name of the comint process.")

(defvar xslt-process-comint-process nil
  "The XSLT debugger process.")

;; Setup the main keymap
(define-key xslt-process-mode-map
  xslt-process-key-binding 'xslt-process-invoke)
(define-key xslt-process-mode-map
  xslt-process-toggle-debug-mode 'xslt-process-toggle-debug-mode)

;; Setup the keymap used for debugging
(define-key xslt-process-debug-mode-map
  xslt-process-key-binding 'xslt-process-invoke)
(define-key xslt-process-debug-mode-map
  xslt-process-toggle-debug-mode 'xslt-process-toggle-debug-mode)
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
  'xslt-process-enable/disable-breakpoint)

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
      :active (and xslt-process-debug-mode
		   (not (xslt-process-is-breakpoint
			 (xslt-process-new-breakpoint-here))))]
     ["Delete breakpoint" xslt-process-delete-breakpoint
      :active (and xslt-process-debug-mode
		   (xslt-process-is-breakpoint
		    (xslt-process-new-breakpoint-here)))]
     ["Disable breakpoint" xslt-process-enable/disable-breakpoint
      :active (and xslt-process-debug-mode
		   (xslt-process-is-breakpoint
		    (xslt-process-new-breakpoint-here)))]
     ["--:singleLine" nil]
     ["Start Debugger" xslt-process-run-debugger :active t]
     ["Step" xslt-process-do-next :active nil]
     ["Next" xslt-process-do-next :active nil]
     ["Finish" xslt-process-do-next :active nil]
     ["Continue" xslt-process-do-next :active nil]))
  (setq xslt-process-mode
	(if (null arg) (not xslt-process-mode)
	  (> (prefix-numeric-value arg) 0)))
  (setq xslt-process-mode-line xslt-process-mode-line-string)
  ;; Force modeline to redisplay
  (xslt-process-update-mode-line))

(defun xslt-process-toggle-debug-mode ()
  "*Setup a buffer in the XSLT debugging mode.
This essentially makes the buffer read-only and binds various keys to
different actions for faster operations."
  (interactive)
  (if xslt-process-debug-mode
      ;; Disable the debug mode if it's enabled
      (progn
	(setq xslt-process-debug-mode nil)
	(toggle-read-only 0)
	(xslt-process-change-breakpoints-highlighting nil)
	(xslt-process-setup-minor-mode xslt-process-mode-map
				       xslt-process-mode-line-string))
    ;; Enable the debug mode
    (setq xslt-process-debug-mode t)
    (toggle-read-only 1)
    (xslt-process-change-breakpoints-highlighting t)
    (xslt-process-setup-minor-mode xslt-process-debug-mode-map
				   xslt-process-debug-mode-line-string)))

(defun xslt-process-quit-debug ()
  "*Quit the debugger and exit from the xslt-process mode."
  (interactive)
  (xslt-process-toggle-debug-mode))

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

(defun xslt-process-new-breakpoint-here ()
  "Returns a breakpoint object at filename and line number of the
current buffer or nil otherwise. By default the breakpoint is enabled."
  (let ((filename (buffer-file-name))
	(line (save-excursion (progn (end-of-line) (count-lines 1 (point))))))
    (cons filename line)))

(defun xslt-process-intern-breakpoint (breakpoint state)
  "Interns BREAKPOINT into the internal hash-table that keeps track of
breakpoints. STATE should be either t for an enabled breakpoint, or
nil for a disabled breakpoint."
  (puthash breakpoint (if state 'enabled 'disabled) xslt-process-breakpoints))

(defun xslt-process-is-breakpoint (breakpoint)
  "Checks whether BREAKPOINT is setup in buffer at line."
  (not (eq (gethash breakpoint xslt-process-breakpoints) nil)))

(defun xslt-process-breakpoint-is-enabled (breakpoint)
  "Returns t if BREAKPOINT is enabled, nil otherwise. Use
xslt-process-is-breakpoint before calling this method to find out
whether BREAKPOINT is a breakpoint or not. This method returns nil
either when the breakpoint doesn't exist or when the breakpoint is
disabled."
  (eq (gethash breakpoint xslt-process-breakpoints) 'enabled))

(defun xslt-process-remove-breakpoint (breakpoint)
  "Remove BREAKPOINT from the internal data structures."
  (let ((filename (xslt-process-breakpoint-filename breakpoint))
	(line (xslt-process-breakpoint-line breakpoint)))
    (remhash (cons filename line) xslt-process-breakpoints)))

(defun xslt-process-breakpoint-filename (breakpoint)
  "Returns the filename of the BREAKPOINT."
  (car breakpoint))

(defun xslt-process-breakpoint-line (breakpoint)
  "Return the line number of the BREAKPOINT."
  (cdr breakpoint))

(defun xslt-process-set-breakpoint ()
  "*Set a breakpoint at line in the current buffer or print an error
message if a breakpoint is already setup."
  (interactive)
  (let* ((breakpoint (xslt-process-new-breakpoint-here))
	 (filename (xslt-process-breakpoint-filename breakpoint))
	 (line (xslt-process-breakpoint-line breakpoint)))
    (if (xslt-process-is-breakpoint breakpoint)
	(message (format "Breakpoint already set in %s at %s" filename line))
      (xslt-process-intern-breakpoint breakpoint t)
      (xslt-process-highlight-line 'xslt-process-enabled-breakpoint-face)
      (message (format "Set breakpoint in %s at %s." filename line)))))

(defun xslt-process-delete-breakpoint ()
  "*Remove the breakpoint at current line in the selected buffer."
  (interactive)
  (let* ((breakpoint (xslt-process-new-breakpoint-here))
	 (filename (xslt-process-breakpoint-filename breakpoint))
	 (line (xslt-process-breakpoint-line breakpoint)))
    (if (xslt-process-is-breakpoint breakpoint)
	(progn
	  (xslt-process-remove-breakpoint breakpoint)
	  (xslt-process-highlight-line 'default)
	  ;; Remove the extent that exists at this line
	  (save-excursion
	    (let* ((from (or (beginning-of-line) (point)))
		   (extent (extent-at from)))
	      (delete-extent extent)))
	  (message (format "Removed breakpoint in %s at %s." filename line)))
      (message (format "No breakpoint in %s at %s" filename line)))))

(defun xslt-process-enable/disable-breakpoint ()
  "*Enable or disable the breakpoint at the current line in buffer, depending
on its state."
  (interactive)
  (let* ((breakpoint (xslt-process-new-breakpoint-here))
	 (filename (xslt-process-breakpoint-filename breakpoint))
	 (line (xslt-process-breakpoint-line breakpoint)))
    (if (xslt-process-is-breakpoint breakpoint)
	(progn
	  ;; Toggle the state of the breakpoint
	  (xslt-process-intern-breakpoint
	   breakpoint
	   (not (xslt-process-breakpoint-is-enabled breakpoint)))
	  ;; Change the face to visually show the status of the
	  ;; breakpoint and print informative message
	  (xslt-process-highlight-line
	   (if (xslt-process-breakpoint-is-enabled breakpoint)
	       (and
		 (message (format "Enabled breakpoint in %s at %s"
				  filename line))
		 'xslt-process-enabled-breakpoint-face)
	     (and
	      (message (format "Disabled breakpoint in %s at %s"
			       filename line))
	      'xslt-process-disabled-breakpoint-face))))
      (message (format "No breakpoint in %s at %s" filename line)))))

(defun xslt-process-change-breakpoints-highlighting (flag)
  "Highlights or unhighlights, depending on FLAG, all the breakpoints
in the current buffer. It doesn't affect the current state of the
breakpoints."
  (maphash
   (lambda (breakpoint state)
     (save-excursion
       (let* ((filename (xslt-process-breakpoint-filename breakpoint))
	      (line (xslt-process-breakpoint-line breakpoint))
	      (from (prog2 (goto-line line) (point)))
	      (extent (or (extent-at from)
			  (make-extent from (prog2 (end-of-line) (point))))))
	 (if flag
	     ;; Highlight the line depending on state
	     (set-extent-face extent
			      (if (eq state 'enabled)
				  'xslt-process-enabled-breakpoint-face
				'xslt-process-disabled-breakpoint-face))
	   (delete-extent extent)))))
   xslt-process-breakpoints))

(defun xslt-process-highlight-line (face)
  "Sets the face of the current line to FACE."
  (save-excursion
    (let* ((from (or (beginning-of-line) (point)))
	   (to (or (end-of-line) (point)))
	   (extent (or (extent-at from)
		       (make-extent from to))))
      (set-extent-face extent face))))


; comint-simple-send (string)
(defun xslt-process-run-debugger ()
  "*Start the XSLT debugger process."
  (let ((buffer
	 (make-comint xslt-process-comint-process-name
		      "java" nil "xslt.debugger.cmdline.Controller")))
    (setq xslt-process-comint-process (get-buffer-process buffer))))

(defun xslt-process-setup-minor-mode (keymap mode-line-string)
  "Setup the XSLT-process minor mode. KEYMAP specifies the keybindings
to be used. MODE-LINE-STRING specifies the string to be displayed in
the modeline."
  (if (fboundp 'add-minor-mode)
      (add-minor-mode 'xslt-process-mode
		      mode-line-string
		      keymap
		      nil
		      'xslt-process-mode)
    (remassoc 'xslt-process-mode minor-mode-alist)
    (or (assoc 'xslt-process-mode minor-mode-alist)
	(setq minor-mode-alist
	      (cons '(xslt-process-mode mode-line-string)
		    minor-mode-alist)))
    (remassoc 'xslt-process-mode minor-mode-map-alist)
    (or (assoc 'xslt-process-mode minor-mode-map-alist)
	(setq minor-mode-map-alist
	      (cons (cons 'xslt-process-mode keymap)
		    minor-mode-map-alist))))
  (force-mode-line-update))

;;;###autoload
(xslt-process-setup-minor-mode xslt-process-mode-map xslt-process-mode-line)

(provide 'xslt-process)
