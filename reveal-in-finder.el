;;; reveal-in-finder.el --- Reveal the file associated with the current buffer in the OS X Finder on Macs.

;; Copyright (C) 2014  Kazuki YOSHIDA

;; Author: Kazuki YOSHIDA (based on "open-finder" found in Stack Overflow.)
;; Keywords: OS X, Finder
;; URL: https://github.com/kaz-yos/elisp/blob/master/reveal-in-finder.el
;; Version: 0.2.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Usage:
;;
;; If M-x reveal-in-finder is invoked in a file-associated buffer,
;; it will open the folder enclosing the file in the OS X Finder.
;; It will also highlight the file the buffer is associated with within the folder.
;;
;; If M-x reveal-in-finder is invoked in a buffer not associated with a file,
;; it will open the folder defined in the default-directory variable.
;; In a dired buffer, this should open the current folder in the OS X Finder.


;;; Code:

;;;###autoload
(defun reveal-in-finder ()
  "Reveal the file associated with the current buffer in the OS X Finder.
In a dired buffer, it will open the current directory."
  (interactive)
  (let* ((path (buffer-file-name))
	dir file)		   ; let* definition ends here.
    (if path
	;; If path has been successfully obtained, set these variables.
	(progn (setq dir (file-name-directory path))
	       (setq file (file-name-nondirectory path)))
      ;; If path is empty, there is no file name. Use the default-directory variable.
      ;; This should work in a dired buffer.
      (setq dir (expand-file-name default-directory)))	
    (reveal-in-finder-as dir file) ; global variables are required to pass it to the helper.
    ))

;; AppleScript helper function. Thanks milkeypostman for suggestions.
;; Use let* to reuse revealpath in defining script.
(defun reveal-in-finder-as (dir file)
  "A helper function for reveal-in-finder.
This function runs the actual AppleScript."
  (let* ((revealpath (if file		   ; define revealpath local variable.
			 (concat dir file) ; dir/file if file name available.
		       dir))		   ; dir only if not.
	 (script			   ; define script variable using revealpath and text.
	  (concat
	   "set thePath to POSIX file \"" revealpath "\"\n"
	   "tell application \"Finder\"\n"
	   " set frontmost to true\n"
	   " reveal thePath \n"
	   "end tell\n")))		   ; let* definitions end here.
    (message script)			   ; check the text output.
    (start-process "osascript-getinfo" nil "osascript" "-e" script)
    ))

(provide 'reveal-in-finder)
;;; reveal-in-finder.el ends here

