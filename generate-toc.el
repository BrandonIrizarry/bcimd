(defun bcimd-generate-toc ()
  "Generate a Table of Contents for the post.

An empty Table of Contents level-1 header must already exist."

  (defvar *toc*
    "End-position of TOC header shared by all functions.")

  (let ((*toc* (save-excursion
                 (goto-char (point-min))
                 (search-forward "# Table of Contents" nil t))))
    (unless *toc*
      (user-error "Missing TOC header"))
    (bcimd-insert-anchors)))

(defun bcimd-insert-anchors ()
  "Insert anchor elements above headers.

Return list of ids."
  (save-excursion
    (goto-char *toc*)

    (let (ids)
      (while (re-search-forward "#[[:space:]]+\\([[:graph:]].*\\)" nil t)
        (let* ((header-start (match-beginning 0))
               (header-content (match-string-no-properties 1))
               (id (mapconcat #'downcase
                              (string-split header-content " ")
                              "-")))
          (save-excursion
            (goto-char header-start)
            (forward-line -1)
            (insert (format "<a id=\"%s\"></a>" id)))
          (push (cons header-content id) ids)))
      (nreverse ids))))


(defun test ()
  (interactive)

  ;; For each heading, prepend an anchor element with the appropriate
  ;; id attribute.
  ;;
  ;; Then save the ids in IDS.
  (let (ids)
    (save-excursion
      (goto-char (point-min))

      ;; Skip past the front matter section.
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
