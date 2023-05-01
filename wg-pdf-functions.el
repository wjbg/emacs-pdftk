;; pdftk

;; change wg/ to pdftk/

(defun wg/readable-pdf-file-p (file)
  "Check if FILE is a readable PDF file."
  (and (file-readable-p file)
       (string= (file-name-extension file) "pdf")))

(defun wg/expand-numbers-string (numbers-str)
  "Expand a string of numbers and ranges into a list of numbers.

The input string NUMBERS-STR should contain a comma-separated list of numbers or
ranges, where a range is specified as a hyphen-separated pair of numbers. For
example, \"1, 3-5, 7\" would be a valid input string.

If a range's start value is greater than its end value, an error is thrown with
the original range string as the error message.

Returns the expanded list of numbers as a space-separated string."
  (let ((numbers-list (split-string numbers-str ", ")))
    (mapconcat
     (lambda (num-str)
       (if (string-match "^\\([0-9]+\\)-\\([0-9]+\\)$" num-str)
           (let ((start (string-to-number (match-string 1 num-str)))
                 (end (string-to-number (match-string 2 num-str))))
             (if (< start end)
                 (mapconcat #'number-to-string (number-sequence start end) " ")
               (error "Invalid range: %s" num-str)))
         num-str))
     numbers-list
     " ")))

(defun wg/range-without-number-string (max numbers-str)
  "Returns numbers from 1 to MAX, without those in NUMBERS-STR.

The function generates a range of numbers from 1 to MAX and filters out any
numbers that are provided in NUMBERS-STR.

The input string NUMBERS-STR should contain a comma-separated list of numbers or
ranges, where a range is specified as a hyphen-separated pair of numbers. For
example, \"1, 3-5, 7\" would be a valid input string.

An error is thrown when any invalid numbers are greater than MAX.

Returns the range of numbers as a space-separated string."
  (let* ((exclude (seq-filter #'identity
			      (mapcar #'string-to-number
				      (split-string (wg/expand-numbers-string numbers-str) " "))))
         (invalid-pages (seq-filter (lambda (x) (> x max)) exclude)))
    (when invalid-pages
      (error "Invalid page number(s): %s" invalid-pages))
    (let ((result (seq-remove (lambda (x) (member x exclude))
                              (number-sequence 1 max))))
      (mapconcat #'number-to-string result " "))))

(defun wg/extract-selected-pdf-pages ()
  "Extract pages from a selected PDF file and save them to a new file.

This function is intended to be used from Dired mode. It prompts the user for a
comma-separated list of page numbers or ranges to extract from the selected PDF
file, as well as an output file name. The range is to be specified as a hyphen-
separated pair of page numbers. For example, \"1, 3-5, 7\" would be a valid
input string."
  (interactive)
  (let* ((pdf-file (dired-get-file-for-visit))
         (page-numbers (read-string "Enter page numbers to extract (e.g. '3, 5, 9-10'): "))
	 (default-output-file (concat
			       (file-name-sans-extension (file-name-nondirectory pdf-file))
			       "-extracted-pages.pdf"))
	 (output-file (read-file-name "Output file: " nil nil nil default-output-file)))
    (wg/extract-pdf-pages pdf-file page-numbers output-file)))

(defun wg/extract-pdf-pages (pdf-file page-numbers output-file)
  "Extract specific pages from a PDF file and save them to a new file.

This function takes a PDF-FILE, a comma-separated string PAGE-NUBERS with the page
numbers or ranges to extract, where a range is specified as a hyphen-separated
pair of page numbers, and an OUTPUT-FILE.

As an example, \"1, 3-5, 7\" would be a valid input string for PAGE-NUMBERS.

If the input PDF-FILE is not readable or the page numbers are invalid, an error
is thrown."
  (if (not (wg/readable-pdf-file-p pdf-file))
      (error "Invalid PDF file: %s" pdf-file))
  (if (string-empty-p page-numbers)
      (error "Invalid page numbers: %s" page-numbers))
  (let* ((pdf-file (convert-standard-filename pdf-file))
         (pages (wg/expand-numbers-string page-numbers))
         (quoted-output-file (concat "\"" (convert-standard-filename output-file) "\""))
         (pdftk-command (concat "pdftk \"" pdf-file "\" cat "
                                pages
                                " output " quoted-output-file)))
    (shell-command pdftk-command)
    output-file))

(defun wg/delete-selected-pdf-pages ()
  "Delete pages from a selected PDF file and save to a new file.

This function is intended to be used from Dired mode. It prompts the user for a
comma-separated list of page numbers or ranges to delete from the selected PDF
file, as well as an output file name. The range is to be specified as a hyphen-
separated pair of page numbers. For example, \"1, 3-5, 7\" would be a valid
input string."
  (interactive)
  (let* ((pdf-file (dired-get-file-for-visit))
         (page-numbers (read-string "Enter page numbers to extract (e.g. '3, 5, 9-10'): "))
	 (default-output-file (concat
			       (file-name-sans-extension (file-name-nondirectory pdf-file))
			       "-without-deleted-pages.pdf"))
	 (output-file (read-file-name "Output file: " nil nil nil default-output-file)))
    (wg/delete-pdf-pages pdf-file page-numbers output-file)))

(defun wg/delete-pdf-pages (pdf-file page-numbers output-file)
  "Delete pages from a PDF file and save the result to a new file.

This function takes a PDF-FILE, a comma-separated string PAGE-NUBERS with the page
numbers or ranges to delete, where a range is specified as a hyphen-separated
pair of page numbers, and an OUTPUT-FILE.

As an example, \"1, 3-5, 7\" would be a valid input string for PAGE-NUMBERS.

If the input PDF file is not readable or the page numbers are invalid, an error
is thrown."
  (if (not (wg/readable-pdf-file-p pdf-file))
      (error "Invalid PDF file: %s" pdf-file))
  (if (string-empty-p page-numbers)
      (error "Invalid page numbers: %s" page-numbers))
  (let* ((pdf-file (convert-standard-filename pdf-file))
	 (max-pages (pdf-info-number-of-pages pdf-file))
	 (pages (wg/range-without-number-string max-pages page-numbers))
         (quoted-output-file (concat "\"" (convert-standard-filename output-file) "\""))
         (pdftk-command (concat "pdftk \"" pdf-file "\" cat "
                                pages
                                " output " quoted-output-file)))
    (shell-command pdftk-command)
    output-file))

(defun wg/join-selected-pdfs ()
  "Concatenate selected PDF files in the order in which they were marked.

This function prompts the user for an output file name and then
joins the PDF files marked in a Dired buffer into this new file."
  (interactive)
  (when (wg/check-marked-files-are-pdf)
    (let* ((default-output-file "new-file.pdf")
           (output-file (read-file-name "Output file: " nil nil nil default-output-file))
           (filenames dired-queue))
      (wg/join-pdfs filenames output-file))))

(defun wg/check-marked-files-are-pdf ()
  "Check if all selected files are readable PDF files."
  (interactive)
  (let ((files (dired-get-marked-files)))
    (if (zerop (length files))
        (message "No files selected")
      (let ((all-pdf t))
        (dolist (file files)
	  (message file)
          (unless (wg/readable-pdf-file-p file)
            (setq all-pdf nil)))
        (if all-pdf
            t
          (message "Invalid selection: not all selected files are readable PDF files."))
        all-pdf))))

(defun wg/join-pdfs (filenames output-file)
  "Join PDF files listed in FILENAMES and save them to OUTPUT-FILENAME.

FILENAMES is a list of filenames (as strings) to be joined.

Returns the name of the output file created."
  (let* ((quoted-filenames (mapcar #'(lambda (filename)
                                       (concat "\"" (convert-standard-filename filename) "\""))
                                   filenames))
         (quoted-output-file (concat "\"" (convert-standard-filename output-file) "\""))
         (pdftk-command (concat "pdftk "
				(mapconcat 'identity quoted-filenames " ")
				" cat output " quoted-output-file)))
    (shell-command pdftk-command)
    output-file))

;; insert pdf after page
;; uncompress into buffer
;; rotate all page CW, CCW
;; rotate page range, CW, CCW
;; make handouts from PDF presentation
