(eval-when-compile (require 'cl))

(defcustom confluence-server "" "Server address for the confluence install.")
(defcustom confluence-user "" "Username for confluence.")
(defcustom confluence-password "" "Password for confluence.")
(defcustom confluence-jar "" 
  "Location of the confluence-cli jar file from the 
confluence command line interface package. The CLI 
package is available separately, here:
https://plugins.atlassian.com/plugin/details/284
")


(define-button-type 'confluence-space-button
  'action 'confluence-get-page-list)


(defun confluence-get-page-list (button)
  (message (with-output-to-string (print button))))


(defun* confluence-list-spaces ()
  (interactive)
  (confluence-wipe-buffer)
  (with-confluence-buffer
    (let ((first t))
      (dolist (s (confluence-space-list))
        (message (format "%s" s))
        (destructuring-bind (space name url visibility) s
          (insert (format "%s - %s" space (substring name 1 -1)))
          (make-text-button (line-beginning-position) (line-end-position)
                            :type 'confluence-space-button))
        (insert "\n"))
      (when (and first (string= visibility "personal"))
        (setq first nil)
        (insert "\n")))))


(defun* confluence-space-list ()
  (let ((spaces nil))
    (with-temp-buffer
      (confluence-command "getSpaceList")

      ;; skip the count and the header
      (goto-line 3)
      (while (not (eobp))
        (let ((s (confluence-parse-space)))
          (when s
            (push s spaces)))
        (next-line)))
    (let* ((spacesr (reverse spaces))
           (globalp (lambda (x) (string= "global" (cadddr x))))
           (globals (confluence-filter globalp spacesr))
           (others (confluence-filter 
                    (lambda (x) (not (funcall globalp x))) spacesr)))
      (append globals others))))

      
(defun confluence-filter (condp lst)
  (delq nil (mapcar (lambda (x) (and (funcall condp x) x)) lst)))


(defun confluence-parse-space ()
  (mapcar 'confluence-chomp (split-string (confluence-line-string) ",")))

(defun confluence-chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (let ((s (if (symbolp str) (symbol-name str) str)))
    (replace-regexp-in-string "\\(^[[:space:]\\n]*\\|[[:space:]\\n]*$\\)" "" s)))


(defun confluence-line-string ()
  (buffer-substring (line-beginning-position) (line-end-position)))

(defun* confluence-command (action) 
  (call-process "java" nil t t
                "-jar" confluence-jar 
                "--user" confluence-user
                "--password" confluence-password
                "--server" confluence-server
                "--action" action))


(defmacro* with-confluence-buffer (&rest body)
  (declare (indent defun))
  `(with-buffer (get-buffer-create "*confluence*")
     (save-excursion
       ,@body)))


(defun confluence-show-buffer () 
  (set-window-buffer (selected-window) "*confluence*"))


(defun confluence-wipe-buffer ()
  (with-confluence-buffer
    (kill-buffer nil)))

(provide 'confluence2)