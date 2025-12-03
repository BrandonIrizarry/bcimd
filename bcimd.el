;; -*- lexical-binding: t; -*-


;;;###autoload
(defun bcimd-generate-toc ()
  "Generate a Table of Contents for the post.

An empty Table of Contents level-1 header must already exist."

  (interactive)

  (unless (eq major-mode 'markdown-mode)
    (user-error "Not a Markdown buffer."))

  ;; Find and save the end position of the Table of Contents header.
  ;;
  ;; Use SAVE-EXCURSION in case a user error leaves point at the end
  ;; of the buffer. We then of course have to manually visit TOC.
  (let ((toc (save-excursion
               (goto-char (point-min))
               (search-forward "# Table of Contents" nil t))))
    (unless toc
      (user-error "Missing TOC header"))

    (save-excursion
      (goto-char toc)

      ;; Construct a list of pairs matching header content with its
      ;; equivalent HTML id attribute value. Call this list OBJS.
      (let (objs)
        (while (re-search-forward "#[[:space:]]+\\([[:graph:]].*\\)" nil t)
          (let* ((header-start (match-beginning 0))
                 (content (match-string-no-properties 1))
                 (id (mapconcat #'downcase
                                (string-split content " ")
                                "-")))
            (push (cons content id) objs)

            ;; While we're at it, insert the necessary HTML anchor elements
            ;; above each header.
            (save-excursion
              (goto-char header-start)
              (forward-line -1)
              (insert (format "<a id=\"%s\"></a>" id)))))

        ;; Make sure OBJS is in forward order, since new entries were
        ;; being pushed to the front of the list.
        (setq objs (nreverse objs))

        ;; Insert the Table of Contents entries into the buffer.
        (goto-char toc)
        (forward-line 2)

        (dolist (obj objs)
          (let ((content (car obj))
                (id (cdr obj)))
            (insert (format "+ [%s](#%s)\n" content id))))))))

(provide 'bcimd)
