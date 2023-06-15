;;; pdftk-dired.el --- Dired Integration for PDF Toolkit (pdftk) -*- lexical-binding: t; -*-

;; Author: Wouter Grouve
;; Version: 0.1
;; Package-Requires: ((emacs "24.4") (pdftk "2.02"))
;; Keywords: dired, pdf, pdftk

;; This package provides Dired integration for PDF Toolkit (pdftk), allowing you
;; to perform various operations on PDF files directly from Dired buffers.

;; Features:
;; - Mark and enqueue PDF files for merging
;; - Unmark and dequeue PDF files from the merge queue
;; - Visual indicator in Dired for the position of files in the queue
;; - Extract specific pages from a PDF file
;; - Delete specific pages from a PDF file
;; - Merge selected PDF files in the order in which they were marked

;; Installation:
;; 1. Ensure that `pdftk' command-line tool is installed on your system and
;;    available in the system's PATH.
;; 2. Copy this file to a directory in your `load-path', and add the following
;;    line to your Emacs initialization file:
;;
;;    (require 'pdftk-dired)

;; Note:
;; - This package relies on the `pdftk' command-line tool to perform the PDF
;;   manipulation operations. Make sure it is installed and available in the
;;   system's PATH.

;; Acknowledgements:
;; - Arthur Miller who wrote the `dired-queue' package, which is used to queue
;;   the PDF files for merging.

(require 'dired)

(defvar-local pdftk-queue nil
  "Order of marked files for merging.")

(defvar-local pdftk-queue-overlay-list nil
  "List of overlays for indicating the position in the queue.")

(defvar pdftk-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-m") 'pdftk-mark-enqueue)
    (define-key map (kbd "M-u") 'pdftk-unmark-dequeue)
    (define-key map [remap dired-unmark-all-marks] 'pdftk-unmark-all-dequeue)
  map)
  "Mode map for dired-mark-and-enqueue-mode.")

(defun pdftk-mark-enqueue (arg &optional interactive)
  "Like `dired-mark' but add filename to the queue."
  (interactive (list current-prefix-arg t))
  (dired-mark arg interactive)
  (dolist (file (dired-get-marked-files))
    (add-to-list 'pdftk-queue file 'append))
  (pdftk-update-queue-indicator))

(defun pdftk-unmark-dequeue (arg &optional interactive)
  "Like dired-unmark but remove filename from the queue too."
  (interactive (list current-prefix-arg t))
  (setq pdftk-queue (delete (expand-file-name (dired-get-filename))
                            pdftk-queue))
  (dired-unmark arg interactive)
  (pdftk-update-queue-indicator))

(defun pdftk-unmark-all-dequeue ()
  "Like dired-unmark-all but remove filename from the queue too."
  (interactive)
  (dolist (file (dired-get-marked-files))
    (setq pdftk-queue (delete file pdftk-queue)))
  (pdftk-update-queue-indicator)
  (dired-unmark-all-marks))

(defun pdftk-update-queue-indicator ()
  "Update the visual indicator in Dired for the position in the queue."
  (dolist (overlay pdftk-queue-overlay-list)
    (delete-overlay overlay))
  (setq pdftk-queue-overlay-list nil)
  (let ((queue-number 1))
    (dolist (file pdftk-queue)
      (let ((filename (file-name-nondirectory file)))
        (dired-goto-file file)
        (let ((start (point))
              (end (line-end-position)))
          (let ((overlay (make-overlay start end)))
            (overlay-put overlay 'display (concat "[" (number-to-string queue-number) "] " filename))
            (add-to-list 'pdftk-queue-overlay-list overlay)
            (setq queue-number (1+ queue-number))))))))

(defun pdftk-update-queue-indicator-old ()
  "Update the visual indicator in Dired for the position in the queue."
  (dolist (overlay pdftk-queue-overlay-list)
    (delete-overlay overlay))
  (setq pdftk-queue-overlay-list nil)
  (let ((queue-number 1))
    (dolist (file pdftk-queue)
      (let ((filename (file-name-nondirectory file)))
        (dired-goto-file file)
        (let ((start (line-beginning-position))
              (end (line-end-position)))
          (let ((overlay (make-overlay start end)))
            (overlay-put overlay 'display (concat "[" (number-to-string queue-number) "] " filename))
            (overlay-put overlay 'face '(:foreground "red"))
            (add-to-list 'pdftk-queue-overlay-list overlay)
            (setq queue-number (1+ queue-number))))))))

(add-hook 'dired-mode-hook #'pdftk-update-queue-indicator)

(define-minor-mode pdftk-mode
  "Enqueue files in Dired."
  :lighter "Dired with Queue"
  (unless (eq major-mode 'dired-mode)
    (user-error "Not in Dired buffer"))
  ;; make sure we always start with empty qeue
  (setq dired-queue nil))

(defun pdftk-number-of-pages (pdf-file)
  "Returns number of pages in PDF-FILE."
  (when (pdftk-pdf-file-p pdf-file)
    (let* ((max-pages (pdf-info-number-of-pages pdf-file)))
      (pdf-info-close pdf-file)
      max-pages)))

(defun pdftk-quote-filename (filename)
  "Returns FILENAME between double quotes."
  (concat "\"" filename "\""))

(defun pdftk-pdf-file-p (file)
  "Checks if FILE is a readable PDF file."
  (if (and (file-readable-p file)
	   (string= (file-name-extension file) "pdf"))
      t
    (progn
      (message "Invalid PDF file: %s" file)
      nil)))

(defun pdftk-expand-page-string (numbers-str &optional max-pages)
  "Expand a string of numbers and ranges and returns all numbers.

The input string NUMBERS-STR is a comma-separated list of numbers
or ranges, where a range is specified as a hyphen-separated pair
of numbers or 'end'. The optional argument MAX-PAGES must
provided be in case the string contains 'end'.

As an example, the following strings would be valid: \"1, 3-5,
7-9\", or \"1, 4-6, 9-end\".

Returns a list of all numbers represented by the string."
  (let* ((numbers-list (split-string numbers-str ", ")))
    (apply #'append
    (mapcar
     (lambda (num-str)
       (if (or (string-match "^\\([0-9]+\\)-\\([0-9]+\\)$" num-str)
	       (string-match "^\\([0-9]+\\)-end$" num-str))
	   (pdftk-expand-range-string num-str max-pages)
	 (list (string-to-number num-str))))
     numbers-list))))

(defun pdftk-expand-range-string (range-str &optional max-pages)
  "Expands range string into a list of numbers.

The input string RANGE-STR is a hyphen-separated pair of numbers
or 'end'. The optional argument MAX-PAGES must provided be in
case the string contains 'end'.

As an example, the following strings would be valid: \"3-5\", or
\"9-end\".

Returns a list of all numbers represented by the string."
  (let* ((start (string-to-number (car (split-string range-str "-"))))
	 (end-str (downcase (car (last (split-string range-str "-")))))
	 (end (if (string= end-str "end") max-pages (string-to-number end-str))))
    (if (and (< start end)
	     (> start 0))
	(number-sequence start end)
      (message "Invalid range: %s" range-str))))

(defun pdftk-expand-exclude-page-string (numbers-str max-pages)
  "Returns numbers from 1 to MAX-PAGES, without those in NUMBERS-STR.

The function generates a range of numbers from 1 to MAX-PAGES and
filters out any numbers that are provided in NUMBERS-STR. The
input string NUMBERS-STR is a comma-separated list of numbers or
ranges, where a range is specified as a hyphen-separated pair of
numbers or 'end'. For example, \"1, 3-5, 7\" or \"1, 5-end\"
would be valid input strings.

Returns a list with all numbers."
  (let ((exclude (pdftk-expand-page-string numbers-str max-pages)))
    (when (listp exclude)
      (seq-remove (lambda (x) (member x exclude))
		  (number-sequence 1 max-pages)))))

(defun pdftk-format-page-numbers (page-numbers)
  "Formats a list of numbers into a space separated string."
  (mapconcat #'number-to-string page-numbers " "))

(defun pdftk-dired-extract-pages ()
  "Extract pages from a selected PDF file and save to a new file.

Used in Dired mode. Prompts for a comma-separated list of page numbers or
ranges to extract from the selected PDF file, and an output file name to save
the pages to PDF to."
  (interactive)
  (let* ((pdf-file (dired-get-file-for-visit))
	 (max-pages (pdftk-number-of-pages pdf-file))
         (numbers-str (read-string "Enter page numbers to extract (e.g. '3, 9-11, 15-end'): "))
	 (page-numbers (pdftk-expand-page-string numbers-str max-pages))
	 (default-output-file (concat
			       (file-name-sans-extension (file-name-nondirectory pdf-file))
			       "-extracted-pages.pdf"))
	 (output-file (read-file-name "Output file: " nil nil nil default-output-file)))
    (pdftk-extract-pages pdf-file page-numbers output-file)))

(defun pdftk-extract-pages (pdf-file page-numbers output-file)
  "Extract PAGE-NUMBERS from PDF-FILE and save to OUTPUT-FILE.

PDF-FILE is the path to the input PDF file. PAGE-NUMBERS is a
list of page numbers that should be extracted from the PDF.
OUTPUT-FILE is the path where the new PDF with the extracted
pages should be saved.

This function requires 'pdftk' command-line tool to be installed
and available in the system's PATH.

Example: (pdftk-extract-pages
             \"/path/to/input.pdf\"
             '(2 4 6) \"/path/to/output.pdf\")

This example will extract pages 2, 4, and 6 from the input.pdf
file and save these as output.pdf."
  (when (pdftk-pdf-file-p pdf-file)
    (let ((pdf-file (convert-standard-filename pdf-file))
          (page-numbers-str (pdftk-format-page-numbers page-numbers))
          (output-file (convert-standard-filename output-file))
          (pdftk-command (concat "pdftk " (pdftk-quote-filename pdf-file)
				 " cat " page-numbers-str
				 " output " (pdftk-quote-filename output-file))))
      (shell-command pdftk-command)
      output-file)))

(defun pdftk-dired-delete-pages ()
  "Delete pages from a selected PDF file and save to a new file.

Used in Dired mode. Prompts for a comma-separated list of page numbers or
ranges to delete from the selected PDF file, and an output file name to save
the modified PDF to."
  (interactive)
  (let* ((pdf-file (dired-get-file-for-visit))
	 (max-pages (pdftk-number-of-pages pdf-file))
         (numbers-str (read-string "Enter page numbers to delete (e.g. '3, 9-11, 15-end'): "))
	 (page-numbers (pdftk-expand-exclude-page-string numbers-str max-pages))
	 (default-output-file (concat
			       (file-name-sans-extension (file-name-nondirectory pdf-file))
			       "-without-deleted-pages.pdf"))
	 (output-file (read-file-name "Output file: " nil nil nil default-output-file)))
    (pdftk-delete-pages pdf-file page-numbers output-file)))


(defun pdftk-delete-pages (pdf-file page-numbers output-file)
  "Delete PAGE-NUMBERS from PDF-FILE and save resulting PDF as OUTPUT-FILE.

PDF-FILE is the path to the input PDF file. PAGE-NUMBERS is a
list of page numbers that should be deleted from the PDF.
OUTPUT-FILE is the path where the modified PDF should be saved.

This function requires 'pdftk' command-line tool to be installed
and available in the system's PATH.

Example: (pdftk-delete-pages
             \"/path/to/input.pdf\"
             '(2 4 6) \"/path/to/output.pdf\")

This example will delete pages 2, 4, and 6 from the input.pdf file and save the
modified PDF as output.pdf."
  (when (pdftk-pdf-file-p pdf-file)
    (let* ((pdf-file (convert-standard-filename pdf-file))
	   (page-numbers-str (pdftk-format-page-numbers page-numbers))
           (output-file (convert-standard-filename output-file))
           (pdftk-command (concat "pdftk " (pdftk-quote-filename pdf-file)
				  " cat " page-numbers-str
				  " output " (pdftk-quote-filename output-file))))
      (shell-command pdftk-command))
    output-file))

(defun pdftk-dired-merge ()
  "Merge selected PDF files in the order in which they were marked.

Used in Dired mode. This function prompts the user for an output
file name and then merges the marked PDF files into this new file."
  (interactive)
  (when (pdftk-check-marked-files-are-pdf)
    (let* ((default-output-file "new-file.pdf")
           (output-file (read-file-name "Output file: " nil nil nil default-output-file))
           (filenames pdftk-queue))
      (pdftk-merge filenames output-file))))

(defun pdftk-check-marked-files-are-pdf ()
  "Checks if all selected files are readable PDF files."
  (let ((files (dired-get-marked-files)))
    (if (zerop (length files))
        (message "No files selected")
      (let ((all-pdf t))
        (dolist (file files)
	  (message file)
          (unless (pdftk-pdf-file-p file)
            (setq all-pdf nil)))
        (if all-pdf
            t
          (message "Invalid selection: not all selected files are readable PDF files."))
        all-pdf))))

(defun pdftk-merge (filenames output-file)
  "Merge PDF files listed in FILENAMES and save them to OUTPUT-FILENAME.

FILENAMES is a list of filenames to be merged, while OUTPUT-FILE
is the path where the new PDF should be saved.

This function requires 'pdftk' command-line tool to be installed
and available in the system's PATH.

Example: (pdftk-merge
             '(\"/path/to/input_1.pdf\", \"/path/to/input_2.pdf\")
             \"/path/to/output.pdf\")

This example merge input_1.pdf and input_2.pdf and save the
result as output.pdf."
  (let* ((filenames (mapcar #'convert-standard-filename filenames))
	 (quoted-filenames (mapcar #'pdftk-quote-filename filenames))
         (output-file (convert-standard-filename output-file))
         (pdftk-command (concat "pdftk "
				(mapconcat 'identity quoted-filenames " ")
				" cat output " (pdftk-quote-filename output-file))))
    (shell-command pdftk-command)
    output-file))

(defun pdftk-split (pdf-file page)
  "Split PDF file after provided page and save both as new files.

PDF-FILE is the path to the PDF file that will be split. PAGE is
an integer that indicates the page number after which the file
should be split. The two output files will have the same name as
the PDF-FILE but with `-1` and `-2` appended before the extension.

This function requires 'pdftk' command-line tool to be installed
and available in the system's PATH.

Example: (pdftk-split \"/path/to/input.pdf\" 8)

This example will split input.pdf after page 8 and save the
resulting pdf files as input-1.pdf and input-2.pdf."
  (let* ((pdf-file (convert-standard-filename pdf-file))
	 (output-file-1 (concat (file-name-sans-extension pdf-file) "-1.pdf"))
	 (output-file-2 (concat (file-name-sans-extension pdf-file) "-2.pdf"))
	 (range-1 (pdftk-expand-page-string (concat "1-" (number-to-string page))))
	 (range-2 (pdftk-expand-page-string (concat (number-to-string page) "-" (number-to-string (pdf-info-number-of-pages pdf-file)))))
         (pdftk-command-1 (concat "pdftk " (pdftk-quote-filename pdf-file) " cat " range-1
				  " output " (pdftk-quote-filename output-file-1)))
         (pdftk-command-2 (concat "pdftk " (pdftk-quote-filename pdf-file) " cat " range-2
				  " output " (pdftk-quote-filename output-file-2))))
    (shell-command pdftk-command-1)
    (shell-command pdftk-command-2)))

(defun pdftk-dired-split ()
  "Split selected PDF file in two files.

Used in Dired mode. This function prompts the user for a page
number after which the file should be split. The two output files
will have the same name as the original pdf file but with `-1`
and `-2` appended before the extension."
  (interactive)
  (let* ((pdf-file (dired-get-file-for-visit))
	 (max-pages (pdftk-number-of-pages pdf-file))
         (page (string-to-number (read-string "Split PDF after page: "))))
    (if (<= page max-pages)
	(pdftk-split pdf-file page)
      (message "Provided page number is larger than number of pages in selected PDF."))))

(defun pdftk-insert (pdf-file pdf-to-insert page output-file)
  "Inserts PDF in another PDF after PAGE and saves as OUTPUT-FILE.

PDF-FILE is the path to PDF file, while PDF-TO-INSERT is the path
the the file that needs to be inserted. PAGE is an integer that
indicates after which page number PDF-TO-INSERT should be
inserted. The resulting file will be saved as OUTPUT-FILE.

This function requires 'pdftk' command-line tool to be installed
and available in the system's PATH.

Example: (pdftk-split \"/path/to/input.pdf\"
            \"/path/to/file_to_insert.pdf\" 8 \"output.pdf\")

This example will insert file_to-insert.pdf in input.pdf after
page 8 and save the resulting pdf files as output_pdf."
  (when (and (pdftk-pdf-file-p pdf-file)
	     (pdftk-pdf-file-p pdf-to-insert))
    (let* ((pdf-file (convert-standard-filename pdf-file))
	   (pdf-to-insert (convert-standard-filename pdf-to-insert))
	   (max-pages (pdftk-number-of-pages pdf-file))
           (output-file (convert-standard-filename output-file))
           (pdftk-command (concat "pdftk "
				  "A=" (pdftk-quote-filename pdf-file) " "
				  "B=" (pdftk-quote-filename pdf-to-insert) " cat "
				  "A1-" (number-to-string page) " B " "A" (number-to-string (1+ page)) "-end "
                                  "output " (pdftk-quote-filename output-file))))
      (if (>= page max-pages)
	  (pdftk-merge (list pdf-file pdf-to-insert) output-file)
	(shell-command pdftk-command)))
    output-file))

(defun pdftk-dired-insert ()
  "Insert a PDF file into another using pdftk.

Used in Dired mode. This function first prompts the user for a
file to insert into the selected pdf file, and the a page number
to indicate where the file needs to be inserted."
  (interactive)
  (let* ((pdf-file (dired-get-file-for-visit))
	 (max-pages (pdftk-number-of-pages pdf-file))
	 (pdf-to-insert (read-file-name "File to insert: " nil nil nil ".pdf"))
         (page (read-string "Insert after page number (integer or 'end'): "))
	 (default-output-file (concat
			       (file-name-sans-extension (file-name-nondirectory pdf-file))
			       "-with-inserted-file.pdf"))
	 (output-file (read-file-name "Output file: " nil nil nil default-output-file)))
    (cond
     ((string= (downcase page) "end")
      (pdftk-insert pdf-file pdf-to-insert max-pages output-file))
     ((and (string-match-p "^[0-9]+$" page)
	   (> (string-to-number page) 0))
      (pdftk-insert pdf-file pdf-to-insert (string-to-number page) output-file))
     (t
      (message "Not a valid page number. Input should be a positive number or 'end'.")))
      output-file))


(defun pdftk-rotate-cw (pdf-file page-numbers output-file)
  "Rotates PAGE numbers in PDF clockwise and saves as OUTPUT-FILE.

PDF-FILE is the path to PDF file, while PAGE-NUBERS is a list of
page numbers to rotate. The resulting file will be saved as
OUTPUT-FILE.

This function requires 'pdftk' command-line tool to be installed
and available in the system's PATH.

Example: (pdftk-rotate-cw \"/path/to/input.pdf\"
            (8 10) \"output.pdf\")

This example will rotate page 8 and 10 in input.pdf and saves the
results as output_pdf."
  (when (pdftk-pdf-file-p pdf-file)
    (let* ((pdf-file (convert-standard-filename pdf-file))
	   (page-range (number-sequence 1 (pdftk-number-of-pages pdf-file)))
	   (page-str (mapconcat #'(lambda (page)
				    (if (member page page-numbers)
					(concat (number-to-string page) "east")
				      (number-to-string page)))
				page-range " "))
           (output-file (convert-standard-filename output-file))
           (pdftk-command (concat "pdftk "
				  (pdftk-quote-filename pdf-file)
				  " cat " page-str " "
                                  "output " (pdftk-quote-filename output-file))))
      (shell-command pdftk-command)))
    output-file)

(defun pdftk-rotate-ccw (pdf-file page-numbers output-file)
  "Rotates PAGE numbers in PDF counter-clockwise and saves as OUTPUT-FILE.

PDF-FILE is the path to PDF file, while PAGE-NUBERS is a list of
page numbers to rotate. The resulting file will be saved as
OUTPUT-FILE.

This function requires 'pdftk' command-line tool to be installed
and available in the system's PATH.

Example: (pdftk-rotate-cw \"/path/to/input.pdf\"
            (8 10) \"output.pdf\")

This example will rotate page 8 and 10 in input.pdf and saves the
results as output_pdf."
  (when (pdftk-pdf-file-p pdf-file)
    (let* ((pdf-file (convert-standard-filename pdf-file))
	   (page-range (number-sequence 1 (pdftk-number-of-pages pdf-file)))
	   (page-str (mapconcat #'(lambda (page)
				    (if (member page page-numbers)
					(concat (number-to-string page) "west")
				      (number-to-string page)))
				page-range " "))
           (output-file (convert-standard-filename output-file))
           (pdftk-command (concat "pdftk "
				  (pdftk-quote-filename pdf-file)
				  " cat " page-str " "
                                  "output " (pdftk-quote-filename output-file))))
      (shell-command pdftk-command)))
    output-file)


(defun pdftk-dired-rotate-cw ()
  "Rotate pages from selected PDF in clockwise direction.

Used in Dired mode. Prompts for a comma-separated list of page numbers or
ranges to rotate in the selected PDF file, and an output file name to save
the modified PDF to."
  (interactive)
  (let* ((pdf-file (dired-get-file-for-visit))
	 (max-pages (pdftk-number-of-pages pdf-file))
         (numbers-str (read-string "Enter page numbers to rotate (e.g. '3, 9-11, 15-end'): "))
	 (page-numbers (pdftk-expand-exclude-page-string numbers-str max-pages))
	 (default-output-file (concat
			       (file-name-sans-extension (file-name-nondirectory pdf-file))
			       "-rotated.pdf"))
	 (output-file (read-file-name "Output file: " nil nil nil default-output-file)))
    (pdftk-rotate-cw pdf-file page-numbers output-file))
  output-file)

(defun pdftk-dired-rotate-ccw ()
  "Rotate pages from selected PDF in counter-clockwise direction.

Used in Dired mode. Prompts for a comma-separated list of page numbers or
ranges to rotate in the selected PDF file, and an output file name to save
the modified PDF to."
  (interactive)
  (let* ((pdf-file (dired-get-file-for-visit))
	 (max-pages (pdftk-number-of-pages pdf-file))
         (numbers-str (read-string "Enter page numbers to rotate (e.g. '3, 9-11, 15-end'): "))
	 (page-numbers (pdftk-expand-exclude-page-string numbers-str max-pages))
	 (default-output-file (concat
			       (file-name-sans-extension (file-name-nondirectory pdf-file))
			       "-rotated.pdf"))
	 (output-file (read-file-name "Output file: " nil nil nil default-output-file)))
    (pdftk-rotate-ccw pdf-file page-numbers output-file))
  output-file)

(defun pdftk-dired-add-watermark ()
  "Add a watermark to the selected PDF.

Used in Dired mode. Prompts for a PDF file that will be inserted
as a watermark, and an output file name to save the new PDF to."
  (interactive)
  (let* ((pdf-file (dired-get-file-for-visit))
	 (watermark (read-file-name "Select PDF watermark file: " nil nil nil ".pdf"))
	 (default-output-file (concat
			       (file-name-sans-extension (file-name-nondirectory pdf-file))
			       "-rotated.pdf"))
	 (output-file (read-file-name "Output file: " nil nil nil default-output-file)))
    (pdftk-add-watermark pdf-file watermark output-file))
  output-file)

(defun pdftk-add-watermark (pdf-file watermark-pdf output-file)
  "Add a watermark to a PDF-FILE and saves as OUTPUT-FILE.

PDF-FILE is the path to PDF file, while WATERMARK-PDFis the path
the the file with the watermark. The resulting file will be saved
as OUTPUT-FILE.

This function requires 'pdftk' command-line tool to be installed
and available in the system's PATH.

Example: (pdftk-add-watermark \"/path/to/input.pdf\"
            \"/path/to/watermark.pdf\" \"output.pdf\")

This example will apply the watermark in watermark.pdf to all
pages in input.pdf and save the resulting pdf files as
output_pdf."
  (when (and (pdftk-pdf-file-p pdf-file)
	     (pdftk-pdf-file-p watermark-pdf))
    (let ((pdf-file (convert-standard-filename pdf-file))
	  (watermark-pdf (convert-standard-filename watermark-pdf))
	  (output-file (convert-standard-filename output-file))
	  (pdftk-command (concat "pdftk " (pdftk-quote-filename pdf-file)
				 " background " (pdftk-quote-filename watermark-pdf)
				 " output " (pdftk-quote-filename output-file))))
      (shell-command pdftk-command)
      output-file)))

(defun pdftk-metadata (pdf-file)
  "Display the metadata of a PDF file using pdftk.

PDF-FILE is the path to PDF file. The information will displayed
in an Emacs buffer.

This function requires 'pdftk' command-line tool to be installed
and available in the system's PATH."
  (when (pdftk-pdf-file-p pdf-file)
    (let ((buffer-name "*PDF MetaData Buffer*")
	  (pdf-file (convert-standard-filename pdf-file))
	  (pdftk-command (concat "pdftk "
				 (pdftk-quote-filename pdf-file)
				 " dump_data output -")))
      (with-current-buffer (get-buffer-create buffer-name)
	(erase-buffer)
	(call-process-shell-command pdftk-command nil buffer-name)
	(switch-to-buffer buffer-name)))))

(defun pdftk-dired-metadata ()
  "Display the metadata of a PDF file using pdftk.

Used in Dired mode. Outputs the metadat to an Emacs buffer."
  (interactive)
  (let* ((pdf-file (dired-get-file-for-visit)))
    (pdftk-metadata pdf-file)))

(provide 'pdftk-dired)

;;; pdftk-dired.el ends here
