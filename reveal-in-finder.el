;;; reveal-in-finder.el --- Reveal the file associated with the current buffer in the OS X Finder on Macs.

;; Copyright (C) 2014  Kazuki YOSHIDA 

;; Author: Kazuki YOSHIDA (based on "open-finder" found in Stack Overflow.)
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
;; Special thanks:
;; This is a modified version of the "open-finder" found at the URL below.
;; http://stackoverflow.com/questions/20510333/in-emacs-how-to-show-current-file-in-finder
;; Thank you elemakil and lawlist for introducing this nice piece of code,
;; and Peter Salazar for pointing out a useful link about AppleScript (below).
;; http://stackoverflow.com/questions/11222501/finding-a-file-selecting-it-in-finder-issue
;; 
;; What this does:
;; If M-x reveal-in-finder is invoked in a file-associated buffer,
;; it will open the folder enclosing the file in the OS X Finder.
;; It will also select the file the buffer is associated with within the folder.
;;
;; If M-x reveal-in-finder is invoked in a buffer not associated with a file,
;; it will open the folder defined in the default-directory variable.
;; In a dired buffer, this should open the current folder in the OS X Finder.


;;; Dependencies
;;
;; None. It only works on the Mac platform with AppleScript support.


;;; Installation
;;
;; If you have auto-install.el, you can do the following to install.
;; (auto-install-from-url "https://raw.github.com/kaz-yos/elisp/master/reveal-in-finder.el")
;;
;; Otherwise you can download the file from the URL below and place it somewhere in your path.
;; https://raw.github.com/kaz-yos/elisp/master/reveal-in-finder.el
;; 
;; Then, put the following in your emacs configuration file.
;; (require 'reveal-in-finder)


;;; Use
;;
;; From a file-associated buffer, just M-x reveal-in-finder RET.
;; It will take you to the enclosing folder in the OS X Finder, highlighting the file.
;; 
;; From a dired buffer, also just M-x reveal-in-finder RET.
;; It will take you to the folder, highlighting the folder itself.


;;; Code:

;;;###autoload
;; autoload macro

;; Function to obtain the path to the file and/or the folder.
(defun reveal-in-finder ()
"Reveal the file associated with the current buffer in the OS X Finder.
In a dired buffer, it will open the current directory."
  (interactive)
  (let ((path (buffer-file-name))
	dir file)
    (if path
	;; If path has been successfully obtained, set these variables.
	(progn (setq dir (file-name-directory path))
	       (setq file (file-name-nondirectory path)))
      ;; If path is empty, there is no file name. Use the default-directory variable.
      ;; This should work in a dired buffer.
      (setq dir (expand-file-name default-directory))
      )
    ;; (message (concat "Opening in Finder: " dir file))	; Show the file name
    (reveal-in-finder-1 dir file)      
    ))

;; Function to open it in Finder.
(defun reveal-in-finder-1 (dir file)
"A helper function for reveal-in-finder"
  (let ((script
	 (if file
	     ;; If it is a file, open the enclosing folder, and select the file.
	     (concat
	      "set thePath to POSIX file \"" (concat dir file) "\"\n"
	      "tell application \"Finder\"\n"
	      " set frontmost to true\n"
	      " reveal thePath \n"
	      "end tell\n"
	      )
	   ;; If it is a folder, open the folder, and select the folder itself..
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

