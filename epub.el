(defvar epub-mode-hook nil)

(defvar epub-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "M-n") 'epub-next-section)
    (define-key map (kbd "M-p") 'epub-previous-section)
    map)
  "Keymap for epub")

(defun epub-next-section ()
  "Proceed to the next section"

  (interactive)

  (if (< current-chapter number-of-sections)
      (setq current-chapter (+ current-chapter 1)))

  (epub-open-section))

(defun epub-open-file ()
  "Open an .epub file"

  (interactive)

  (setq epub-file (read-file-name "Choose an .epub file: "))

  (setq epub-directory (concat "/tmp/epub-mode/" (number-to-string (random 10000))))

  ;; extract files
  (start-process "" nil "unzip" epub-file "-d" epub-directory)

  (sleep-for 10)

  ;; find the .ncx file
  (setq find-ncx (concat "find " epub-directory " -regex" " .*.ncx"))
  (setq ncxfile (shell-command-to-string find-ncx))
  (setq ncxfile (replace-regexp-in-string "\n" "" ncxfile))

  ;; create a file manifest from the ncx file
  (setq manifest '())
  (with-current-buffer (find-file-noselect ncxfile)
    (goto-char (point-min))
    (while (re-search-forward "src=\".*\"" nil t)
      (setq match (match-string-no-properties 0))
      (setq match (replace-regexp-in-string "src=\"" "" match))
      (setq match (replace-regexp-in-string "\"$" "" match))
      (add-to-list 'manifest match)))
  (setq manifest (reverse manifest))

  ;; define some important variables
  (setq number-of-sections (length manifest))
  (setq current-chapter 0)
  (setq relative-directory (file-name-directory (directory-file-name ncxfile)))

  (epub-open-section))

(defun epub-open-section ()
  "Open a section"

  (interactive)

  ;; the directory relative to which the file list is defined
  (setq current-file (nth current-chapter manifest))
  (setq current-path (concat relative-directory current-file))

  (erase-buffer)
  (call-process "w3m" nil t nil current-path)
  (goto-char (point-min)))

(defun epub-previous-section ()
  "Go back to a previous section"

  (interactive)

  (if (> current-chapter 0)
      (setq current-chapter (- current-chapter 1)))

  (epub-open-section))

(defun epub-mode ()
  "A simple epub reader"

  (switch-to-buffer "epub")
  (interactive)
  (kill-all-local-variables)
  (use-local-map epub-mode-map)
  (setq major-mode 'epub-mode)
  (setq mode-name "epub")
  (run-hooks 'epub-mode-hook))

(provide 'epub-mode)
