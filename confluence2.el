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
  'action 'confluence-space-button-press)
(define-button-type 'confluence-page-button
  'action 'confluence-page-button-press)


(defun confluence-space-button-press (button)
  (confluence-list-pages (button-get button :space)))


(defun confluence-page-button-press (button)
  (confluence-edit-page (button-get button :space)
                        (button-get button :page)))


(defun confluence-list-pages (space)
  (interactive "sSpace: ")
  (confluence-wipe-buffer)
  (with-confluence-buffer
    (confluence-command "getPageList" "--space" space)
    (goto-line 1)
    (confluence-kill-whole-line)
    (confluence-parse (lambda ()
                        (let ((line (confluence-line-string)))
                          (make-text-button (line-beginning-position)
                                            (line-end-position)
                                            :space space
                                            :page line
                                            :type 'confluence-page-button))))
    (sort-lines nil (point-min) (point-max)))
  (confluence-show-buffer))


(defun* confluence-edit-page (space page) 
  (interactive "sSpace: \nsPage: ")
  (confluence-wipe-buffer)
  (with-confluence-buffer
    (confluence-command "getSource" "--space" space "--title" page)
    (goto-line 1)
    (confluence-kill-whole-line))
  (confluence-show-buffer))


(defun* confluence-list-spaces ()
  (interactive)
  (confluence-wipe-buffer)
  (with-confluence-buffer
    (let ((first t))
      (dolist (s (confluence-space-list))
        (destructuring-bind (space name url visibility) s
          (when (and first (string= visibility "personal"))
            (setq first nil)
            (insert "\n"))
          (insert (format "%s - %s" space (substring name 1 -1)))
          (make-text-button (line-beginning-position) (line-end-position)
                            :space space
                            :type 'confluence-space-button)
          (insert "\n")))))
  (confluence-show-buffer))


(defun* confluence-space-list ()
  (let ((spaces nil))
    (with-temp-buffer
      (confluence-command "getSpaceList")

      ;; skip the count and the header
      (goto-line 3)
      (setq spaces (confluence-parse 'confluence-parse-space)))
    (let* ((spacesr (reverse spaces))
           (globalp (lambda (x) (string= "global" (cadddr x))))
           (globals (confluence-filter globalp spacesr))
           (others (confluence-filter 
                    (lambda (x) (not (funcall globalp x))) spacesr)))
      (append globals others))))


(defun confluence-parse (parseline)
  (save-excursion
    (let ((results nil))
      (while (not (eobp))
        (if (confluence-line-empty-p)
            (confluence-kill-whole-line)
          (let ((s (funcall parseline)))
            (when s
              (push s results))
            (next-line))))
      results)))


(defun confluence-line-empty-p ()
  (save-excursion
    (beginning-of-line)
    (eolp)))

      
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

(defun* confluence-command (action &rest args) 
  (apply 'call-process "java" nil t t
         "-jar" confluence-jar 
         "--user" confluence-user
         "--password" confluence-password
         "--server" confluence-server
         "--action" action
         args))


(defmacro* with-confluence-buffer (&rest body)
  (declare (indent defun))
  `(with-buffer (get-buffer-create "*confluence*")
     (save-excursion
       ,@body)))


(defun confluence-show-buffer () 
  (set-window-buffer (selected-window) "*confluence*")
  (with-current-buffer "*confluence*"
    (setq buffer-read-only t)))


(defun confluence-wipe-buffer ()
  (with-confluence-buffer
    (kill-buffer nil)))

(defun confluence-kill-whole-line ()
  (let ((kill-whole-line t))
    (kill-line)))




(provide 'confluence2)