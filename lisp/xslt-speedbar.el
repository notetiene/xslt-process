;;;; xslt-speedbar.el -- Provides XSLT debugger functionality in speedbar

;; Package: xslt-process
;; Author: Ovidiu Predescu <ovidiu@cup.hp.com>
;; Created: April 3, 2000
;; Time-stamp: <April  9, 2001 03:16:01 ovidiu>
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


;;;; Speedbar support
(speedbar-add-supported-extension ".xml")
(speedbar-add-supported-extension ".xsl")

(require 'speedbar)

(or (boundp 'speedbar-dynamic-tags-function-list)
    (error "Speedbar 0.11 or newer is required to use xslt-speedbar."))

(defface xslt-process-speedbar-enabled-breakpoint-face
  '((((class color) (background light))
     (:foreground "salmon")))
  "*Face used to highlight enabled breakpoints."
  :group 'font-lock-highlighting-faces)

(defface xslt-process-speedbar-disabled-breakpoint-face
  '((((class color) (background light))
     (:foreground "wheat4")))
  "*Face used to highlight enabled breakpoints."
  :group 'font-lock-highlighting-faces)

(defvar xslt-process-speedbar-keymap (speedbar-make-specialized-keymap)
  "Specialized keymap to handle special keys in the speedbar while in
the XSLT debugging mode.")

(define-key xslt-process-speedbar-keymap "x"
  (lambda () (interactive)
      (speedbar-change-initial-expansion-list "XSLT process")))
(define-key xslt-process-speedbar-keymap "+" 'speedbar-expand-line)
(define-key xslt-process-speedbar-keymap "=" 'speedbar-expand-line)
(define-key xslt-process-speedbar-keymap "-" 'speedbar-contract-line)

(speedbar-add-expansion-list
 '("XSLT process"
   xslt-process-easymenu-definition
   xslt-process-speedbar-keymap
   xslt-process-speedbar-buttons))

(defvar xslt-process-speedbar-bufname " SPEEDBAR"
  "The name of the speedbar buffer.")

(defvar xslt-process-easymenu-definition nil
  "Menu entries to be displayed when the XSLT debugger speedbar mode
is selected.")

(defvar xslt-process-top-level-entries
  '(("Breakpoints"
     xslt-process-speedbar-show-breakpoints
     (eq (hashtable-fullness xslt-process-breakpoints) 0))
    ("Source frames"
     xslt-process-speedbar-show-source-frames-stack
     (null xslt-process-source-frames-stack))
    ("Style frames"
     xslt-process-speedbar-show-style-frames-stack
     (null xslt-process-style-frames-stack))
    ("Global variables"
     xslt-process-speedbar-show-global-variables
     t)
    ("Local variables"
     xslt-process-speedbar-show-local-variables
     t))
  "Top level entries in the speedbar. Each entry contains a list of
the name of the entry, the function to be invoked to expand the entry
and an expression to test whether the list of items is empty.")

(mapc
 (lambda (entry)
   (let* ((name (car entry))
	  (varname (concat "xslt-process-"
			   (replace-in-string (downcase name) " " "-")
			   "-item-expanded")))
     (eval (read (concat "(setq " varname " nil)")))))
 xslt-process-top-level-entries)

(add-hook 'xslt-process-breakpoint-set-hooks
	  'xslt-process-speedbar-update-breakpoints)
(add-hook 'xslt-process-breakpoint-removed-hooks
	  'xslt-process-speedbar-update-breakpoints)
(add-hook 'xslt-process-breakpoint-enabled/disabled-hooks
	  'xslt-process-speedbar-update-breakpoints)
(add-hook 'xslt-process-source-frames-changed-hooks
	  'xslt-process-speedbar-source-frames-changed)
(add-hook 'xslt-process-style-frames-changed-hooks
	  'xslt-process-speedbar-style-frames-changed)

(defun xslt-process-speedbar-frame-mode ()
  "Called from menu to display the speedbar."
  (interactive)
  (speedbar-frame-mode)
  (speedbar-change-initial-expansion-list "XSLT process"))

(defun xslt-process-speedbar-update-breakpoints ()
  "Function called whenever the breakpoint list changes as a result of
adding, removing, enabling or disabling a breakpoint. Redisplays the
list of breakpoints with the appropriate faces."
  (if xslt-process-breakpoints-item-expanded
      (speedbar-with-writable
	(save-excursion
	  (set-buffer (get-buffer xslt-process-speedbar-bufname))
	  (beginning-of-buffer)
	  (xslt-process-select-menu-item 0 "Breakpoints")
	  (speedbar-delete-subblock 0)
	  (forward-line 1)
	  (xslt-process-speedbar-show-breakpoints "" 0)))))

(defun xslt-process-select-menu-item (indent text)
  "Searches for TEXT at INDENT level in the speedbar menu and
positions the point right at the beginning of the line."
  (beginning-of-buffer)
  (re-search-forward (format "%s:.*%s" indent text) (point-max) t))

(defun xslt-process-speedbar-buttons (directory zero)
  "Create the buffer to displays the XSLT debugger menu in speedbar."
  (mapc
   (lambda (entry)
     (let* ((name (car entry)))
       (speedbar-make-tag-line 'angle ?+ 'xslt-process-show-top-level-entry
			       name
			       name 'xslt-process-top-level-entry-click
			       name
			       'speedbar-button-face 0)))
   xslt-process-top-level-entries))

(defun xslt-process-top-level-entry-click (text token indent)
  "When the user clicks on the text of a top level entry in speedbar."
  (save-excursion
    (let* ((beg (progn (beginning-of-line) (point)))
	   (end (progn (end-of-line) (point)))
	   (line (buffer-string beg end)))
      (message "xslt-process-top-level-entry-click '%s' '%s' '%s' '%s'" text token indent line)
      (xslt-process-show-top-level-entry line token indent))))

(defun xslt-process-show-top-level-entry (text token indent)
  "Function called to show the breakpoints."
  (message "xslt-process-show-top-level-entry '%s' '%s' '%s'" text token indent)
  (let* ((entry (assoc token xslt-process-top-level-entries))
	 (name (car entry))
	 (fn (cadr entry))
	 (condition (caddr entry))
	 (varname (concat "xslt-process-"
			  (replace-in-string (downcase name) " " "-")
			  "-item-expanded")))
    (cond ((string-match "+" text)	;; expand the breakpoints
	   (eval (read (concat "(setq " varname " t)")))
	   (if (eval condition)
	       (message "No %s." (downcase name))
	     (speedbar-change-expand-button-char ?-)
	     (speedbar-with-writable
	       (save-excursion
		 (end-of-line) (forward-char 1)
		 (funcall fn text indent)))))
	  ((string-match "-" text)		;; contract this node
	   (speedbar-change-expand-button-char ?+)
	   (speedbar-delete-subblock indent)
	   (eval (read (concat "(setq " varname " nil)"))))
	  (t (error "Ooops...  not sure what to do"))))
  (speedbar-center-buffer-smartly)
  (save-excursion (speedbar-stealthy-updates)))

(defun xslt-process-speedbar-show-breakpoints (text indent)
  "Function called from `xslt-process-show-top-level-entry' to display
the breakpoints in speedbar."
  (let ((breakpoints))
    (maphash
     (lambda (breakpoint enabled)
       (let ((filename (file-name-nondirectory
			(xslt-process-breakpoint-filename breakpoint)))
	     (line (xslt-process-breakpoint-line breakpoint))
	     (enabled (xslt-process-breakpoint-is-enabled breakpoint)))
	 (setq breakpoints (cons breakpoint breakpoints))))
     xslt-process-breakpoints)
    (setq breakpoints
	  (sort breakpoints
		(lambda (b1 b2)
		  (let ((file1 (file-name-nondirectory
				   (xslt-process-breakpoint-filename b1)))
			(line1 (xslt-process-breakpoint-line b1))
			(file2 (file-name-nondirectory
				   (xslt-process-breakpoint-filename b2)))
			(line2 (xslt-process-breakpoint-line b2)))
		    (and (or (string< file1 file2) (string= file1 file2))
			 (< line1 line2))))))
    (mapc
     (lambda (bkpt)
       (let* ((filename (xslt-process-breakpoint-filename bkpt))
	      (line (xslt-process-breakpoint-line bkpt))
	      (enabled (xslt-process-breakpoint-is-enabled bkpt))
	      (face (if enabled
			'xslt-process-speedbar-enabled-breakpoint-face
		      'xslt-process-speedbar-disabled-breakpoint-face)))
	 (speedbar-make-tag-line 'bracket ?? 'xslt-process-show-breakpoint
				 bkpt
				 (format  "%s:%s"
					  (file-name-nondirectory filename)
					  line)
				 'xslt-process-show-breakpoint
				 bkpt
				 face
				 (1+ indent))))
     breakpoints)))

(defun xslt-process-show-breakpoint (text breakpoint indent)
  "Show the buffer that contains BREAKPOINT."
  (let* ((filename (xslt-process-breakpoint-filename breakpoint))
	 (line (xslt-process-breakpoint-line breakpoint)))
    (message "Breakpoint in %s, at line %s" filename line)
    (xslt-process-show-line text (list filename line) indent)))

(defun xslt-process-show-line (text fileline indent)
  "Show the buffer that contains FILELINE."
  (let* ((filename (car fileline))
	 (line (cadr fileline))
	 (buffer (xslt-process-get-file-buffer filename)))
    (save-window-excursion
      (pop-to-buffer buffer)
      (set-buffer buffer)
      (goto-line line))))

(defun xslt-process-speedbar-show-source-frames-stack (text indent)
  "Show the source stack frames."
  (message "show source frames '%s' '%s'" text indent))

(defun xslt-process-speedbar-show-style-frames-stack (text indent)
  "Show the source stack frames."
  (message "show style frames '%s' '%s'" text indent)
  (mapc
   (lambda (frame)
     (let* ((display-name (xslt-process-frame-display-name frame))
	    (filename (xslt-process-frame-file-name frame))
	    (line (xslt-process-frame-line frame))
	    (fileline (list filename line)))
       (speedbar-make-tag-line 'braket ?? 'xslt-process-show-line
			       fileline
			       display-name
			       'xslt-process-show-line
			       fileline
			       'speedbar-file-face
			       (1+ indent))))
   xslt-process-style-frames-stack))

(defun xslt-process-speedbar-source-frames-changed ()
  "Called by the debugger when the source frame stack changes."
  )

(defun xslt-process-speedbar-style-frames-changed ()
  "Called by the debugger when the style frame stack changes."
  (if xslt-process-style-frames-item-expanded
      (speedbar-with-writable
	(save-excursion
	  (set-buffer (get-buffer xslt-process-speedbar-bufname))
	  (beginning-of-buffer)
	  (xslt-process-select-menu-item 0 "Style frames")
	  (speedbar-delete-subblock 0)
	  (forward-line 1)
	  (xslt-process-speedbar-show-style-frames-stack "" 0)))))

(provide 'xslt-speedbar)
