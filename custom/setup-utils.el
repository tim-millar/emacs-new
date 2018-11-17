;; iterm integration functions from:
;; https://sam217pa.github.io/2016/09/01/emacs-iterm-integration/

(defun tm/get-file-dir-or-home ()
  "If inside a file buffer, return the directory, else return home"
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
	"~/"
      (file-name-directory filename))))

(defun tm/iterm-goto-filedir-or-home ()
  "Go to present working dir and focus iterm"
  (interactive)
  (do-applescript
   (concat
    " tell application \"iTerm2\"\n"
    "   tell the current session of current window\n"
    (format "     write text \"cd %s\" \n" (get-file-dir-or-home))
    "   end tell\n"
    " end tell\n"
    " do shell script \"open -a iTerm\"\n"
    ))
  )

(defun tm/iterm-focus ()
  ;xs(interactive)
  (do-applescript
   " do shell script \"open -a iTerm\"\n"
   ))

(provide 'setup-utils)
