;;; reveal-in-finder.el --- Open the file enclosing folder in OS X Finder on Macs.

;; Copyright (C) 2014  Kazuki YOSHIDA

;; Author: Kazuki YOSHIDA
;; Keywords: OS X, Finder
;; URL: https://github.com/kaz-yos/elisp/blob/master/reveal-in-finder.el
;; Version: 0.1.0

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
;; This is a modified version of the original found below.
;; Original: http://stackoverflow.com/questions/20510333/in-emacs-how-to-show-current-file-in-finder
;; 
;; It opens the enclosing folder and highlight the file in OS X Finder if invoked from a file buffer.
;; It highlights the folder if invoked from a buffer which does not have an associated file (dired, etc).

;;; Dependencies
;;
;; None. It only works on the Mac platform.

;;; Installation
;;
;; If you have auto-install.el
;; (auto-install-from-url https://raw.github.com/kaz-yos/elisp/master/reveal-in-finder.el)
;; 
;; Then, put the following in your emacs configuration file.
;; (require 'reveal-in-finder)

;;; Use
;;
;; From a file-associated buffer, just M-x reveal-in-finder RET.
;; It will take you to the enclosing folder in Finder, highlighting the file.
;; 
;; From a dired buffer, also just M-x reveal-in-finder RET.
;; It will take you to the folder, highlighting the folder itself.



;;; Code:

;; Function to obtain the path to the file and/or the folder.
(defun reveal-in-finder ()
  (interactive)
  (let ((path (buffer-file-name))
	dir file)
    (if path
	;; if path has been successfully obtained.
	(progn (setq dir (file-name-directory path))
	       (setq file (file-name-nondirectory path)))
      ;; if path is empty, there is no file name. Use the default-directory variable
      (setq dir (expand-file-name default-directory))
      )
    ;; (message (concat "Opening in Finder: " dir file))	; Show the file name
    (reveal-in-finder-1 dir file)      
    ))

;; Function to open it in Finder.
(defun reveal-in-finder-1 (dir file)
  (let ((script
	 (if file
	     ;; if it is a file.
	     (concat
	      "set thePath to POSIX file \"" (concat dir file) "\"\n"
	      "tell application \"Finder\"\n"
	      " set frontmost to true\n"
	      " reveal thePath \n"
	      "end tell\n"
	      )
	   ;; if it is a folder.
	   (concat
	    "set thePath to POSIX file \"" (concat dir) "\"\n"
	    "tell application \"Finder\"\n"
	    " set frontmost to true\n"
	    " reveal thePath \n"
	    "end tell\n"))))
    ;; (message script)	; Show the script in the mini-buffer
    (start-process "osascript-getinfo" nil "osascript" "-e" script)
    ))

(provide 'reveal-in-finder)
;;; reveal-in-finder.el ends here

