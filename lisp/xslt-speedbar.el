;;;; xslt-speedbar.el -- Provides XSLT debugger functionality in speedbar

;; Package: xslt-process
;; Author: Ovidiu Predescu <ovidiu@cup.hp.com>
;; Created: April 3, 2000
;; Time-stamp: <April  3, 2001 17:11:43 ovidiu>
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
  '(("Breakpoints" . xslt-process-show-breakpoints)
    ("Source frames" . xslt-process-show-source-stack-frame)
    ("Style frames" . xslt-process-show-style-stack-frame)
    ("Global variables" . xslt-process-show-global-variables))
  "Top level entries in the speedbar and their associated functions.")

(defun xslt-process-speedbar-buttons (directory zero)
  "Create the buffer to displays the XSLT debugger menu in speedbar."
  (mapc
   (lambda (entry)
     (let ((name (car entry))
	   (fn (cdr entry)))
       (speedbar-make-tag-line 'angle ?+ 'xslt-process-show-top-level-entry
			     name
			     name fn name
			     'speedbar-button-face 0)))
   xslt-process-top-level-entries))

(defun xslt-process-top-level-entry-click (text token indent)
  "When the user clicks on the breakpoint's button in speedbar."
  (message "xslt-process-top-level-entry-click '%s' '%s' '%s'" text token indent))

(defun xslt-process-show-top-level-entry (text token indent)
  "Function called to show the breakpoints."
  (message "xslt-process-top-level-entry '%s' '%s' '%s'" text token indent)
  (cond ((string-match "+" text);; expand the breakpoints
	 ;; Change the appearance of the entry
	 (if (eq (hashtable-fullness xslt-process-breakpoints) 0)
	     (message "No %s." (downcase token))
	   (speedbar-change-expand-button-char ?-)
	   (speedbar-with-writable
	     (save-excursion
;	       (end-of-line) (forward-char 1)
	       (message "maphash starts")
	       (maphash
		(lambda (breakpoint enabled)
		  (speedbar-insert-button
		   (format "%s:%s"
			   (file-name-nondirectory
			    (xslt-process-breakpoint-filename breakpoint))
			   (xslt-process-breakpoint-line breakpoint))
		   'speedbar-file-face
		   'speedbar-highlight-face
		   'xslt-process-select-breakpoint breakpoint))
		xslt-process-breakpoints)
	       (message "maphash ended")))))
	((string-match "-" text);; contract this node
	 (speedbar-change-expand-button-char ?+)
	 (speedbar-delete-subblock indent))
	(t (error "Ooops...  not sure what to do")))
  (speedbar-center-buffer-smartly))

(defun xslt-process-show-breakpoints (text breakpoint indent)
  "Show the buffer that contains BREAKPOINT."
  (message "show breakpoints '%s' '%s' '%s'" text breakpoint indent))

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
