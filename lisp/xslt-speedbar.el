;;;; xslt-speedbar.el -- Provides XSLT debugger functionality in speedbar

;; Package: xslt-process
;; Author: Ovidiu Predescu <ovidiu@cup.hp.com>
;; Created: April 3, 2000
;; Time-stamp: <April  4, 2001 01:45:19 ovidiu>
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


(defvar xslt-process-speedbar-mode-name "XSLT process"
  "The name of the speedbar major mode that displays the debugger
related info.")

;;;; Speedbar support
(speedbar-add-supported-extension ".xml")
(speedbar-add-supported-extension ".xsl")

(require 'speedbar)

(or (boundp 'speedbar-dynamic-tags-function-list)
    (error "Speedbar 0.11 or newer is required to use sb-texinfo."))

(speedbar-add-expansion-list
 '("XSLT process"
   xslt-process-easymenu-definition
   speedbar-buffers-key-map
   xslt-process-speedbar-buttons))

(defvar xslt-process-easymenu-definition nil
  "Menu entries to be displayed when the XSLT debugger speedbar mode
is selected.")

(defvar xslt-process-top-level-entries
  '(("Breakpoints"
     xslt-process-speedbar-show-breakpoints
     (eq (hashtable-fullness xslt-process-breakpoints) 0))
    ("Source frames"
     xslt-process-show-source-stack-frame
     t)
    ("Style frames"
     xslt-process-show-style-stack-frame
     t)
    ("Global variables"
     xslt-process-show-global-variables
     t))
  "Top level entries in the speedbar. Each entry contains a list of
the name of the entry, the function to be invoked to expand the entry
and an expression to test whether the list of items is empty.")

(defun xslt-process-speedbar-buttons (directory zero)
  "Create the buffer to displays the XSLT debugger menu in speedbar."
  (mapc
   (lambda (entry)
     (let ((name (car entry)))
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
  (cond ((string-match "+" text)	;; expand the breakpoints
	 (let* ((entry (assoc token xslt-process-top-level-entries))
		(name (car entry))
		(fn (cadr entry))
		(condition (caddr entry)))
	   (if (eval condition)
	       (message "No %s." (downcase name))
	     (speedbar-change-expand-button-char ?-)
	     (speedbar-with-writable
	       (save-excursion
		 (end-of-line) (forward-char 1)
		 (funcall fn text indent))))))
	((string-match "-" text)	;; contract this node
	 (speedbar-change-expand-button-char ?+)
	 (speedbar-delete-subblock indent))
	(t (error "Ooops...  not sure what to do")))
  (speedbar-center-buffer-smartly)
  (save-excursion (speedbar-stealthy-updates)))

(defun xslt-process-speedbar-show-breakpoints (text indent)
  "Function called from `xslt-process-show-top-level-entry' to display
the breakpoints in speedbar."
  (maphash
   (lambda (breakpoint enabled)
     (speedbar-make-tag-line 'bracket ?? 'xslt-process-show-breakpoint
			     breakpoint
			     (format 
			      "%s:%s"
			      (file-name-nondirectory
			       (xslt-process-breakpoint-filename breakpoint))
			      (xslt-process-breakpoint-line breakpoint))
			     'xslt-process-show-breakpoint
			     breakpoint
			     'speedbar-file-face
			     (1+ indent)))
   xslt-process-breakpoints))
  

(defun xslt-process-show-breakpoint (text breakpoint indent)
  "Show the buffer that contains BREAKPOINT."
  (let* ((filename (xslt-process-breakpoint-filename breakpoint))
	 (line (xslt-process-breakpoint-line breakpoint))
	 (buffer (xslt-process-get-file-buffer filename)))
    (message "Breakpoint in %s, at line %s" filename line)
    (save-window-excursion
      (pop-to-buffer buffer)
      (set-buffer buffer)
      (goto-line line))))
      

(defun xslt-process-show-source-stack-frame (text token indent)
  "Show the source stack frames."
  (message "show source frames '%s' '%s' '%s'" text token indent))

;; Attach these new functions to handle XSLT-process mode.
;(add-to-list 'speedbar-dynamic-tags-function-list
;	     '(xslt-process-fetch-dynamic . xslt-process-insert-list))

;(defun xslt-process-fetch-dynamic (filename)
;  (message "xslt-process-fetch-dynamic invoked on %s" filename)
;  (let ((buffer (get-file-buffer filename)))
;    (save-excursion
;      (set-buffer buffer)
;      (message "set buffer %s, xslt-process %s" buffer xslt-process-mode)
;      (if (not xslt-process-debug-mode)
;	  t
;	(list '("el 1" . 10) '("el 2" . 20))))))

;(defun xslt-process-insert-list (indent list)
;  (speedbar-insert-generic-list indent list
;				'speedbar-tag-expand
;				'speedbar-tag-find))

(provide 'xslt-speedbar)
