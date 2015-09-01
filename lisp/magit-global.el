;; using magit from other buffers
(global-set-key [(control x) (g) (g)] 'magit-status)
(global-set-key [(control x) (g) (c)] 'magit-commit-popup)
(global-set-key [(control x) (g) ($)] 'magit-process)


(defun my-magit-read-args-and-files (prompt &optional args
                                            files-no-prefix
                                            files-with-prefix)
  (if current-prefix-arg
      (my-magit-s-to-args-and-files
       (p4-read-arg-string prompt
                           (my-magit-args-and-files-to-s
                            args files-with-prefix)))
    (list args files-no-prefix)))


(defun my-magit-log (&optional args files)
  (interactive (my-magit-read-args-and-files
                "git log: "
                magit-log-arguments
                (-if-let (f (my-magit-context-single-filename))
                    (list (expand-file-name f)) nil)
                nil))
  (magit-log-all args files))
(global-set-key [(control x) (g) (l)] 'my-magit-log)


(defun my-magit-context-single-filename ()
  ;; next 2 lines from magit-log-buffer-file
  (or (buffer-file-name (buffer-base-buffer))
      magit-buffer-file-name
      default-directory))

(defun my-magit-s-to-args-and-files (str)
  (let* ((tokens (split-string-and-unquote str))
         (args (--take-while (not (equal it "--")) tokens))
         (files (cdr (--drop-while (not (equal it "--")) tokens))))
    (list args files)))
;; tests
(my-magit-s-to-args-and-files "--all --graph -- ./myfile.txt ./foo/file.txt")
(my-magit-s-to-args-and-files "--all --graph -- ./myfile.txt")
(my-magit-s-to-args-and-files "--all --graph --")
(my-magit-s-to-args-and-files "--all --graph")
(my-magit-s-to-args-and-files "")
(my-magit-s-to-args-and-files "--")
(my-magit-s-to-args-and-files "-- ./myfile.txt")
;; quotes and escaping
;; basic quoting to deal with spaces
(my-magit-s-to-args-and-files "--all -S\"find me\"")
;; this is how you represent a " character, needs both quoting and
;; escaping
(my-magit-s-to-args-and-files "\"foo\\\"bar\"")
;; double quotes can be escaped, but only within double quotes
;; (my-magit-s-to-args-and-files "foo\\\"bar")  ; error
;; single quotes are not supported/special
(my-magit-s-to-args-and-files "'single quote' \"'foo\"")
;; space cannot be escaped, only quotes can be escaped
(my-magit-s-to-args-and-files "escaped\\ space")
;; unbalanced quotes are bad
;; this results in error "End of file during parsing
;; (my-magit-s-to-args-and-files "unbalanced\"quote")
;; escape characters by themselves are not treated specially
(my-magit-s-to-args-and-files "\\ \\")



(defun my-magit-args-and-files-to-s (args files)
  ;; last empty string to get trailing space for filename completion
  ;; to work at point right away
  (combine-and-quote-strings (append args '("--") files '(""))))
;; tests
(my-magit-args-and-files-to-s '("--all" "--graph")
                              '("./myfile.txt" "./foo/file.txt"))
(my-magit-args-and-files-to-s '("--all" "--graph") '("./myfile.txt"))
(my-magit-args-and-files-to-s '("--all" "--graph") nil)
(my-magit-args-and-files-to-s nil nil)
(my-magit-args-and-files-to-s nil '("./myfile.txt" "./foo/file.txt"))
;; things needing quoting
(my-magit-args-and-files-to-s '("-S" "with space" "'single'" "\"double\"") nil)
(my-magit-args-and-files-to-s nil '("file with space" "\\" "'" "\""))
(my-magit-args-and-files-to-s nil '("\""))
(my-magit-args-and-files-to-s nil '("\\"))





(defun my-magit-diff (args files)
  (interactive (my-magit-read-args-and-files
                "git diff: "
                magit-diff-arguments
                (-if-let (f (my-magit-context-single-filename))
                    (list (expand-file-name f)) nil)
                nil))
  (magit-mode-setup magit-diff-buffer-name-format
                    magit-diff-switch-buffer-function
                    #'magit-status-mode
                    #'my-magit-diff-refresh-buffer args files))
(defun my-magit-diff-refresh-buffer (args files)
  ;; note that magit-diff-section-arguments must be included for -/+
  ;; to work in magit-status-mode
  (magit-insert-section (status)
    (magit-insert-section (unstaged)
      (magit-insert-heading "Unstaged changes:")
      (magit-git-wash #'magit-diff-wash-diffs
        "diff" args magit-diff-section-arguments
        "--no-prefix" (and magit-diff-show-diffstat "--stat")
        "--" files))
    (magit-insert-section (staged)
      (magit-insert-heading "Staged changes:")
      (magit-git-wash #'magit-diff-wash-diffs
        "diff" "--cached" args magit-diff-section-arguments
        "--no-prefix" (and magit-diff-show-diffstat "--stat")
        "--" files))
    (magit-section-show-level-4-all)))
(global-set-key [(control x) (g) (=)] 'my-magit-diff)

(defun my-magit-stage ()
  (interactive)
  (magit-stage-item buffer-file-name)
  (message "Staged"))
(global-set-key [(control x) (g) (s)] 'my-magit-stage)

;; equivalent to p4-opened
(defun my-magit-refresh-opened-buffer ()
  (magit-insert-unstaged-changes))
(defun my-magit-opened ()
  (interactive)
  (let* ((dir default-directory)
         (buf (get-buffer-create "*magit-opened*")))
    (magit-mode-setup buf nil
                      #'magit-status-mode 
                      #'my-magit-refresh-opened-buffer)
    (with-current-buffer buf
      (goto-char (point-min))
      (forward-line))
    ))
(global-set-key [(control x) (g) (o)] 'my-magit-opened)



(provide 'magit-global)
