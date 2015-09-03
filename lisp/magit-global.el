;; using magit from other buffers
(global-set-key [(control x) (g) (g)] 'magit-status)
(global-set-key [(control x) (g) (c)] 'magit-commit-popup)
(global-set-key [(control x) (g) ($)] 'magit-process)
(global-set-key [(control x) (g) (l)] 'magit-global-log)
(global-set-key [(control x) (g) (=)] 'magit-global-diff)
(global-set-key [(control x) (g) (-)] 'magit-ediff-show-unstaged)
(global-set-key [(control x) (g) (o)] 'magit-global-opened)
(global-set-key [(control x) (g) (a)] 'magit-global-add)
(global-set-key [(control x) (g) (u)] 'vc-revert) ; for now


(defun magit-global-log (&optional args files)
  (interactive (magit-global-read-args-and-files
                "git log: "
                magit-log-arguments
                (magit-global-current-files) nil))
  ;; TODO: something somewhere is messing with current directory, so
  ;; need to pass absolute file paths to git
  (setq files (mapcar 'expand-file-name files))
  (magit-log-all args files))


(defun magit-global-diff (args files)
  (interactive (magit-global-read-args-and-files
                "git diff: "
                magit-diff-arguments
                (magit-global-current-files) nil))
  ;; TODO: something somewhere is messing with current directory, so
  ;; need to pass absolute file paths to git
  (setq files (mapcar 'expand-file-name files))
  (magit-mode-setup magit-diff-buffer-name-format
                    magit-diff-switch-buffer-function
                    #'magit-status-mode
                    #'magit-global-diff-refresh-buffer args files)
  ;; TODO: this is a hack, this funtionality logically belongs in
  ;; magit-mode-setup.  Shouldn't invoke switch-buffer-function under
  ;; some conditions.
  (if (zerop (buffer-size))
      (progn (quit-window)
             (message "No changes unstaged or staged"))))
(defun magit-global-diff-refresh-buffer (args files)
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


;; equivalent to p4-opened
(defun magit-global-opened ()
  "Meant to function like bs-show (fancier list-buffers) but for
  modified  files."
  (interactive)
  (magit-mode-setup "*magit-opened*" nil
                    #'magit-status-mode 
                    #'magit-global-opened-refresh-buffer)
  (if (zerop (buffer-size))
      (progn (quit-window)
             (message "No modified files"))
    (progn
      ;; position on the first file
      (goto-char (point-min))
      (forward-line)))
  ;; TODO:
  ;; * want to avoid highlighting of section headers when point is on
  ;;   them
  ;; * want to quit-window when a file is selected with Enter
  )
(defun magit-global-opened-refresh-buffer ()
  (magit-insert-section (status)
    (magit-insert-unstaged-changes)
    (magit-insert-staged-changes)))

;; TODO: not using this function yet.  Not sure if this is the right
;; interface.  Also see vc-git-register
(defun magit-global-add (&optional args files)
  (interactive (magit-global-read-args-and-files
                "git add: "
                '("--intent-to-add")
                (magit-global-current-files) nil))
  (interactive)
  ;; TODO: something somewhere is messing with current directory, so
  ;; need to pass absolute file paths to git
  (setq files (mapcar 'expand-file-name files))
  (magit-stage-1 args files)
  ;; TODO: probably want to message something
  )
;;  (message "Staged"))

;; TODO: defun magit-global-revert like vc-revert

;; TODO: still not using this function.  Compared to C-x v v is is
;; awful chatty if I just want to commit this one file right now.
;; Compared magit-commit-popup it lacks useful functionality like
;; rewording
;;
;; TODO: when doing completion here can limit to files with changes
;; (and directories?)
(defun magit-global-commit (&optional args files)
  (interactive (magit-global-read-args-and-files
                "git log: "
                magit-commit-arguments
                (magit-global-current-files) nil))
  ;; TODO: append args and files because magit-commit has different
  ;; signature.  It probably shouldn't.
  (magit-commit (append args '("--") files))
  ;; TODO: modify magit-commit-diff and add `magit-global-commit there
  ;; to see diffs in a window popup here
  ;;
  ;; TODO: to use this with any comfort on one file need to see that
  ;; file more prominently in the commit message buffer.  Also need to
  ;; test if the code inserting comment into that buffer obeys file
  ;; restrictions if passed
  )


(defun magit-global-current-files ()
  "Determine or guess current file.  Return as a list"
  ;; next 2 lines from magit-log-buffer-file
  (-if-let (current-file (or
                          ;; next 2 lines from magit-log-buffer-file
                          (buffer-file-name (buffer-base-buffer))
                          magit-buffer-file-name
                          default-directory))
      (list current-file)
    nil))


(defun magit-global-read-args-and-files (prompt &optional args
                                            files-no-prefix
                                            files-with-prefix)
  (if current-prefix-arg
      (magit-global-s-to-args-and-files
       ;; TODO: what we want here ideally is a full-blown completion
       ;; on git options, their arguments, and files.  Like bash
       ;; completion.  Or like p4-read-arg-string completion for p4
       ;;
       ;; To start with just filename completion would do, and you'd
       ;; think that read-shell-command would work well here, but it
       ;; doesn't.  Doesn't want to complete files after space unless
       ;; the string in the minibuffer starts with something special
       ;; like 'make' for example.  But once you type something like
       ;; ./ it does work.
       ;;
       ;; To roll our own MVP all that's needed is the right
       ;; completion function to use in completing read.  Something
       ;; like read-file-name-internal but that can handle other text
       ;; before the filename. You'd think it would be simple, but
       ;; it's currely beyond me.
       ;;
       ;;   (completing-read "git log: "
       ;;   'completion-function-to-be-written nil nil "initial input"
       ;;   'magit-global-history-variable)
       ;;
       ;; So, for now let's use p4-read-arg-string if available, and
       ;; fallback to read-shell-command otherwise.
       (if (functionp 'p4-read-arg-string)
           (p4-read-arg-string prompt
                               (magit-global-args-and-files-to-s
                                args files-with-prefix))
         (read-shell-command prompt (magit-global-args-and-files-to-s
                                args files-with-prefix))))
    (list args files-no-prefix)))


(defun magit-global-s-to-args-and-files (str)
  (let* ((tokens (split-string-and-unquote str))
         (args (--take-while (not (equal it "--")) tokens))
         (files (cdr (--drop-while (not (equal it "--")) tokens))))
    (list args files)))
;; tests
(magit-global-s-to-args-and-files "--all --graph -- ./myfile.txt ./foo/file.txt")
(magit-global-s-to-args-and-files "--all --graph -- ./myfile.txt")
(magit-global-s-to-args-and-files "--all --graph --")
(magit-global-s-to-args-and-files "--all --graph")
(magit-global-s-to-args-and-files "")
(magit-global-s-to-args-and-files "--")
(magit-global-s-to-args-and-files "-- ./myfile.txt")
;; quotes and escaping
;; basic quoting to deal with spaces
(magit-global-s-to-args-and-files "--all -S\"find me\"")
;; this is how you represent a " character, needs both quoting and
;; escaping
(magit-global-s-to-args-and-files "\"foo\\\"bar\"")
;; double quotes can be escaped, but only within double quotes
;; (magit-global-s-to-args-and-files "foo\\\"bar")  ; error
;; single quotes are not supported/special
(magit-global-s-to-args-and-files "'single quote' \"'foo\"")
;; space cannot be escaped, only quotes can be escaped
(magit-global-s-to-args-and-files "escaped\\ space")
;; unbalanced quotes are bad
;; this results in error "End of file during parsing
;; (magit-global-s-to-args-and-files "unbalanced\"quote")
;; escape characters by themselves are not treated specially
(magit-global-s-to-args-and-files "\\ \\")


(defun magit-global-args-and-files-to-s (args files)
  ;; last empty string to get trailing space for filename completion
  ;; to work at point right away
  (combine-and-quote-strings (append args '("--") files '(""))))
;; tests
(magit-global-args-and-files-to-s '("--all" "--graph")
                              '("./myfile.txt" "./foo/file.txt"))
(magit-global-args-and-files-to-s '("--all" "--graph") '("./myfile.txt"))
(magit-global-args-and-files-to-s '("--all" "--graph") nil)
(magit-global-args-and-files-to-s nil nil)
(magit-global-args-and-files-to-s nil '("./myfile.txt" "./foo/file.txt"))
;; things needing quoting
(magit-global-args-and-files-to-s '("-S" "with space" "'single'" "\"double\"") nil)
(magit-global-args-and-files-to-s nil '("file with space" "\\" "'" "\""))
(magit-global-args-and-files-to-s nil '("\""))
(magit-global-args-and-files-to-s nil '("\\"))


(provide 'magit-global)


;; TODO
;; * REFACTOR: adjust to "call magit-mode-get-buffer with nil as FORMAT"
;;   https://github.com/magit/magit/commit/ce2f4f90d1267ccf87197dce45d357919103c8d3
;; * USABILITY: git rebase -i with unstaged changes - should say
;;   what's wrong in echo area
;; * BUG: do -diff in a file not under git - still opens the window
;; * FEATURE: git add a file
;; * BUG: in magit calling save-some-buffers messages "(No buffers
;;   need saving)" which is annoying.  Separately, magit tries to
;;   auto-save way to offten
;; * BUG: magit-ediff-show-unstaged should kill buffers/windows based
;;   on buffer name, not position (reproduce with ~)
