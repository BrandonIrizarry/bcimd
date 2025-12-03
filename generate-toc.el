(defun test ()
  (interactive)

  ;; For each heading, prepend an anchor element with the appropriate
  ;; id attribute.


  ;; Grab all the ids.
  (let (ids)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "<a id=\"\\([[:graph:]-]+\\)\"></a>" nil t)
        (push (match-string-no-properties 1) ids)))

    ;; Make sure we have them in the correct order.
    (setq ids (reverse ids))

    ;; Insert the corresponding list item inside the Table of
    ;; Contents.
    (let ((pos (save-excursion
                 (goto-char (point-min))
                 (search-forward "# Table of Contents" nil t))))
      (unless pos
        (user-error "Missing '# Table of Contents' header"))

      (goto-char pos)
      (forward-line 2)

      ;; Insert the TOC entries!
      (dolist (id ids)
        (let ((proper (mapconcat #'capitalize (string-split id "-") " ")))
          (insert (format "+ [%s](#%s)\n" proper id)))))))
