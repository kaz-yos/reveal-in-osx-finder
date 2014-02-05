Miscellaneous emacs lisp files
=====

reveal-in-finder
-----

**Usage:**

- If ```M-x reveal-in-finder``` is invoked in a file-associated buffer, it will open the folder enclosing the file in the OS X Finder.  It will also select the file the buffer is associated with within the folder.

- If ```M-x reveal-in-finder``` is invoked in a buffer not associated with a file,it will open the folder defined in the default-directory variable. In a dired buffer, this should open the current folder in the OS X Finder.


**Installation**

There is no dependencies, except that it only works on the OS X environment on Macs.

When it becomes available on MELPA, do the following, then choose and install reveal-in-finder.

```
M-x list-packages
```

If you prefer auto-install.el, you can do the following to install.
```lisp
(auto-install-from-url "https://raw.github.com/kaz-yos/elisp/master/reveal-in-finder.el")
```

Otherwise you can download the file from the URL below and place it somewhere in your path.
https://raw.github.com/kaz-yos/elisp/master/reveal-in-finder.el

Then, put the following in your emacs configuration file.

```lisp
(require 'reveal-in-finder)
```

**Special thanks:**

This is a modified version of the ```open-finder``` found at the URL below. Thank you elemakil and lawlist for introducing this nice piece of code,

http://stackoverflow.com/questions/20510333/in-emacs-how-to-show-current-file-in-finder

and Peter Salazar for pointing out a useful link about AppleScript (below),

http://stackoverflow.com/questions/11222501/finding-a-file-selecting-it-in-finder-issue

and mikeypostman and purcell for auditing the code for MELPA approval.
