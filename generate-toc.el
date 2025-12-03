(defun bcimd-generate-toc ()
  "Generate a Table of Contents for the post.

An empty Table of Contents level-1 header must already exist."

  ;; Find and save the end position of the Table of Contents header.
  (let ((toc (save-excursion
               (goto-char (point-min))
               (search-forward "# Table of Contents" nil t))))
    (unless toc
      (user-error "Missing TOC header"))

    (save-excursion
      (goto-char toc)

      (let (objs)
        (while (re-search-forward "#[[:space:]]+\\([[:graph:]].*\\)" nil t)
          (let* ((header-start (match-beginning 0))
                 (content (match-string-no-properties 1))
                 (id (mapconcat #'downcase
                                (string-split content " ")
                                "-")))
            (save-excursion
              (goto-char header-start)
              (forward-line -1)
              (insert (format "<a id=\"%s\"></a>" id)))
            (push (cons content id) objs)))

        (setq objs (nreverse objs))

        ;; Insert the TOC entries!
        (goto-char toc)
        (forward-line 2)

        (dolist (obj objs)
          (let ((content (car obj))
                (id (cdr obj)))
            (insert (format "+ [%s](#%s)\n" content id))))))))
