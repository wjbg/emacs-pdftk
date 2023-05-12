;; pdftk

(defun pdftk-quote-filename (filename)
  "Returns FILENAME between double quotes."
  (concat "\"" filename "\""))

(defun pdftk-readable-pdf-file-p (file)
  "Check if FILE is a readable PDF file."
  (if (and (file-readable-p file)
	   (string= (file-name-extension file) "pdf"))
      t
    (progn
      (message "Invalid PDF file: %s" file)
      nil)))

;; use this function to check; is this really still necessary?
(defun pdftk-valid-page-numbers-p (number-list pdf-file)
  "Checks if the provided number-list is valid for PDF-FILE."
  (let* ((max-pages (pdf-info-number-of-pages pdf-file))
	 (max-number (string-to-number
		      (last (split-string number-list)))))
    (if (<= max-number max-pages)
	t
      (progn
	(message "Maximum number in range exceeds page count: %s" pdf-file)
	nil))))

(defun pdftk-expand-numbers-string (numbers-str &optional max-pages)
  "Expand a string of numbers and ranges.

The input string NUMBERS-STR should contain a comma-separated list of numbers or
ranges, where a range is specified as a hyphen-separated pair of numbers or 'end'. For
example, \"1, 3-5, 7-end\" would be a valid input string.

An error is thrown if a range's start value is greater than its end value.

If the optional argument MAX-PAGES is provided, it is used as the maximum value for the 'end' keyword.

Returns the expanded list of numbers as a space-separated string."
  (let* ((numbers-list (split-string numbers-str ", ")))
    (mapconcat
     (lambda (num-str)
       (if (string-match "^\\([0-9]+\\)-\\([0-9]+\\)$" num-str)
           (let ((start (string-to-number (match-string 1 num-str)))
                 (max-pages (string-to-number (match-string 2 num-str))))
             (if (< start max-pages)
                 (mapconcat #'number-to-string (number-sequence start max-pages) " ")
               (error "Invalid range: %s" num-str)))
         (if (string-match "^\\([0-9]+\\)-end$" num-str)
             (let ((start (string-to-number (match-string 1 num-str)))
                   (max-pages (or max-pages (pdf-info-number-of-pages pdf-file))))
               (if (< start max-pages)
                   (mapconcat #'number-to-string (number-sequence start max-pages) " ")
                 (error "Invalid range: %s" num-str)))
           num-str)))
     numbers-list
     " ")))

(defun pdftk-range-without-number-string (numbers-str max-pages)
  "Returns numbers from 1 to MAX-PAGES, without those in NUMBERS-STR.

The function generates a range of numbers from 1 to MAX-PAGES and filters out any
numbers that are provided in NUMBERS-STR. The input string NUMBERS-STR should
contain a comma-separated list of numbers or ranges, where a range is
specified as a hyphen-separated pair of numbers. For example, \"1, 3-5, 7\"
would be a valid input string.

An error is thrown when any invalid numbers are greater than MAX-PAGES.

Returns the resulting numbers as a space-separated string."
  (let* ((exclude (seq-filter #'identity
			      (mapcar #'string-to-number
				      (split-string (pdftk-expand-numbers-string numbers-str) " "))))
         (invalid-pages (seq-filter (lambda (x) (> x max-pages)) exclude)))
    (when invalid-pages
      (error "Invalid page number(s): %s" invalid-pages))
    (let ((result (seq-remove (lambda (x) (member x exclude))
                              (number-sequence 1 max-pages))))
      (mapconcat #'number-to-string result " "))))

(defun pdftk-dired-extract-pages ()
  "Extract pages from a selected PDF file and save them to a new file.

This function is used from Dired mode. It prompts the user for a comma-
separated list of page numbers or ranges to extract from the selected PDF
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
    (pdftk-extract-pages pdf-file page-numbers output-file)))

(defun pdftk-extract-pages (pdf-file page-numbers output-file)
  "Extract specific pages from a PDF file and save them to a new file.

This function takes a PDF-FILE, a comma-separated string PAGE-NUBERS with the page
numbers or ranges to extract, where a range is specified as a hyphen-separated
pair of page numbers, and an OUTPUT-FILE. As an example, \"1, 3-5, 7\" would be a
valid input string for PAGE-NUMBERS."
  (when (pdftk-readable-pdf-file-p pdf-file)
    (let* ((pdf-file (convert-standard-filename pdf-file))
	   (max-pages (pdf-info-number-of-pages pdf-file))
           (pages (pdftk-expand-numbers-string page-numbers max-pages))
           (output-file (convert-standard-filename output-file))
           (pdftk-command (concat "pdftk " (pdftk-quote-filename pdf-file) " cat "
                                  pages " output " (pdftk-quote-filename output-file))))
      (shell-command pdftk-command))
    output-file))

(defun pdftk-dired-delete-pages ()
  "Delete pages from a selected PDF file and save to a new file.

This function is used from Dired mode. It prompts the user for a comma-separated
list of page numbers or ranges to delete from the selected PDF file, as well as
 an output file name. The range is to be specified as a hyphen-separated pair of
 page numbers. For example, \"1, 3-5, 7\" would be a valid input string."
  (interactive)
  (let* ((pdf-file (dired-get-file-for-visit))
         (page-numbers (read-string "Enter page numbers to extract (e.g. '3, 5, 9-10'): "))
	 (default-output-file (concat
			       (file-name-sans-extension (file-name-nondirectory pdf-file))
			       "-without-deleted-pages.pdf"))
	 (output-file (read-file-name "Output file: " nil nil nil default-output-file)))
    (pdftk-delete-pages pdf-file page-numbers output-file)))

(defun pdftk-delete-pages (pdf-file page-numbers output-file)
  "Delete pages from a PDF file and save the result to a new file.

This function takes a PDF-FILE, a comma-separated string PAGE-NUBERS with the page
numbers or ranges to delete, where a range is specified as a hyphen-separated
pair of page numbers, and an OUTPUT-FILE.

As an example, \"1, 3-5, 7\" would be a valid input string for PAGE-NUMBERS.

If the input PDF file is not readable or the page numbers are invalid, an error
is thrown."
  (when (pdftk-readable-pdf-file-p pdf-file)
    (let* ((pdf-file (convert-standard-filename pdf-file))
	   (max-pages (pdf-info-number-of-pages pdf-file))
	   (pages (pdftk-range-without-number-string page-numbers max-pages))
           (output-file (convert-standard-filename output-file))
           (pdftk-command (concat "pdftk " (pdftk-quote-filename pdf-file) " cat "
                                  pages " output " (pdftk-quote-filename output-file))))
      (shell-command pdftk-command))
    output-file))

(defun pdftk-dired-merge ()
  "Concatenate selected PDF files in the order in which they were marked.

This function prompts the user for an output file name and then
joins the PDF files marked in a Dired buffer into this new file."
  (interactive)
  (when (pdftk-check-marked-files-are-pdf)
    (let* ((default-output-file "new-file.pdf")
           (output-file (read-file-name "Output file: " nil nil nil default-output-file))
           (filenames dired-queue))
      (pdftk-merge filenames output-file))))

(defun pdftk-check-marked-files-are-pdf ()
  "Check if all selected files are readable PDF files."
  (interactive)
  (let ((files (dired-get-marked-files)))
    (if (zerop (length files))
        (message "No files selected")
      (let ((all-pdf t))
        (dolist (file files)
	  (message file)
          (unless (pdftk-readable-pdf-file-p file)
            (setq all-pdf nil)))
        (if all-pdf
            t
          (message "Invalid selection: not all selected files are readable PDF files."))
        all-pdf))))






;; (quoted-filenames (mapcar #'(lambda (filename)
;;                                        (concat "\"" (convert-standard-filename filename) "\""))
;;                                    filenames))

;;
;;


(defun pdftk-merge (filenames output-file)
  "Join PDF files listed in FILENAMES and save them to OUTPUT-FILENAME.

FILENAMES is a list of filenames (as strings) to be joined.

Returns the name of the output file created."
  (let* ((filenames (mapcar #`convert-standard-filename filenames))
	 (quoted-filenames (mapcar #'pdftk-quote-filename filenames))
         (output-file (convert-standard-filename output-file))
         (pdftk-command (concat "pdftk "
				(mapconcat 'identity quoted-filenames " ")
				" cat output " (pdftk-quote-filename output-file))))
    (shell-command pdftk-command)
    output-file))

(defun pdftk-split (pdf-file page)
  "Split PDF file after provided page and save both as new files.

This function takes a PDF-FILE and an integer PAGE as an input.
The PDF-FILE will be split after PAGE and the two output files
will have the same name as the `-1` and `-1` appended before the
extension."
  (let* ((pdf-file (convert-standard-filename pdf-file))
	 (output-file-1 (concat (file-name-sans-extension pdf-file) "-1.pdf"))
	 (output-file-2 (concat (file-name-sans-extension pdf-file) "-2.pdf"))
	 (range-1 (pdftk-expand-numbers-string (concat "1-" (number-to-string page))))
	 (range-2 (pdftk-expand-numbers-string (concat (number-to-string page) "-" (number-to-string (pdf-info-number-of-pages pdf-file)))))
         (pdftk-command-1 (concat "pdftk " (pdftk-quote-filename pdf-file) " cat " range-1
				  " output " (pdftk-quote-filename output-file-1)))
         (pdftk-command-2 (concat "pdftk " (pdftk-quote-filename pdf-file) " cat " range-2
				  " output " (pdftk-quote-filename output-file-2))))
    (shell-command pdftk-command-1)
    (shell-command pdftk-command-2)))

(defun pdftk-dired-split ()
  "Split selected PDF file in two files.

This function is intended to be used from Dired mode. It prompts
the user for a page number after which the file should be
splitted. The two output files will have the same name as the
`-1` and `-1` appended before the extension."
  (interactive)
  (let* ((pdf-file (dired-get-file-for-visit))
	 (max-pages (pdf-info-number-of-pages pdf-file))
         (page (string-to-number (read-string "Split PDF after page: "))))
    (if (<= page max-pages)
	(pdftk-split pdf-file page)
      (message "Provided page number is larger than number of pages in selected PDF."))))

(defun pdftk-insert-after-page (pdf-file file-to-insert page)
  ""
  nil)

(defun pdftk-dired-insert-after-page ()
  ""
  nil)

(defun pdftk-rotate-cw (pdf-file page-numbers)
  ""
  nil)

(defun pdftk-dired-rotate-cw ()
  ""
  nil)

(defun pdftk-rotate-ccw (pdf-file page-numbers)
  ""
  nil)

(defun pdftk-dired-rotate-ccw ()
  ""
  nil)

(defun pdftk-add-watermark (pdf-file watermark-pdf)
  ""
  nil)

(defun pdftk-metadata (pdf-file)
  "Outputs metadata to temp buffer"
  nil)

(defun pdftk-dired-metadata (pdf-file)
  "Outputs metadata to temp buffer"
  nil)


;; make handouts from PDF presentation; use shuffle

;; idea: wg/fill-grade-form using fdf format...
