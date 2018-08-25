(defvar epub-mode-hook nil)

(defvar epub-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "M-n") 'epub-next-section)
    (define-key map (kbd "M-o") 'epub-open-file)
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

  ;; extract files
  (setq epub-directory (concat "/tmp/epub-mode/" (number-to-string (random 10000))))
  (start-process "" nil "unzip" epub-file "-d" epub-directory)
  (sleep-for 10) ;; temporary, to make sure the files are unzipped before it tries to open them

  ;; find the .opf file
  (setq find-opf (concat "find " epub-directory " -regex" " .*.opf"))
  (setq opf-file (shell-command-to-string find-opf))
  (setq opf-file (replace-regexp-in-string "\n" "" opf-file))

  ;; create a file manifest from the opf file
  (setq manifest '())
  (with-current-buffer (find-file-noselect opf-file)
    (goto-char (point-min))
    (while (re-search-forward "href=\".*?\"" nil t)
      (setq match (match-string-no-properties 0))
      (setq match (replace-regexp-in-string "href=\"" "" match))
      (setq match (replace-regexp-in-string "\"$" "" match))
      (if (string-match-p ".htm" match)
	  (add-to-list 'manifest match)))
    (setq manifest (reverse manifest)))

  ;; define some important variables
  (setq number-of-sections (length manifest))
  (setq current-chapter 0)
  (setq relative-directory (file-name-directory (directory-file-name opf-file)))

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
