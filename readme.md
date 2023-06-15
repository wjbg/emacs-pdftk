[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![made-with-emacs](https://img.shields.io/badge/made%20for-Emacs-blue)](https://www.gnu.org/software/emacs/)


# pdftk-dired

pdftk-dired is an Emacs Lisp package that provides Dired integration for PDF Toolkit (pdftk). It allows you to perform various operations on PDF files directly from Dired buffers.

## Features

- Mark and enqueue PDF files for merging
- Unmark and dequeue PDF files from the merge queue
- Visual indicator in Dired for the position of files in the queue
- Extract specific pages from a PDF file
- Delete specific pages from a PDF file
- Merge selected PDF files in the order in which they were marked

## Installation

1. Ensure that the `pdftk` command-line tool is installed on your system and available in the system's PATH.
2. Copy the `pdftk-dired.el` file to a directory in your `load-path`.
3. Add the following line to your Emacs initialization file:

```elisp
(require 'pdftk-dired)
```

## Usage
To use pdftk-dired, open a Dired buffer and perform the following actions:

Please note that pdftk-dired relies on the pdftk command-line tool to
perform the PDF manipulation operations. Make sure it is installed and
available in the system's PATH.

#### Merging of PDF files

Mark PDF files for merging:
- Move the cursor to a PDF file and use `pdftk-mark-enqueue` (bound to
  `M-m) to mark and enqueue the file.

Unmark PDF files from the merge queue:
- Move the cursor to a marked PDF file and use `pdftk-unmark-deqeueu`
  (bound to `M-u`) to unmark and dequeue the file.
- To unmark all files in the merge queue, use
  `pdftk-unmark-all-dequeue` or `dired-unmark-all-marks`.

The Dired buffer will display a visual indicator showing the position
of each file in the merge queue. The marked files can be merged using
`pdftk-dired-merge`.

#### Insert or combine PDF files

`pdftk-dired-insert` inserts a PDF file into the PDF file at point.
The user is asked to provide the filename of the PDF file to insert,
and a page number after which the provided PDF file will be inserted,
and a filename to save the new PDF file to..

#### Extract and delete pages from a PDF file

`pdftk-dired-extract-pages` and `pdftk-dired-delete-pages` extract or
delete pages from the PDF file at point. The user is asked for a page
range to extract or delete, and a filename for the new file.

#### Split PDF file

`pdftk-dired-split` splits a PDF file at after a specified page
number. Both new files will be saved afterwards with the same name as
the original PDF but with a `-1` and `-2` appended before the
extension.

#### Rotate pages in PDF file

`pdftk-dired-rotate-cw` and `pdftk-dired-rotate-ccw` rotate specified
pages in the PDF clockwise or counter-clockwise, respectively. The
user is asked to provide the numbers of the pages to rotate and a
filename to save the new PDF file to.

#### Add watermark

`pdftk-dired-add-watermark` adds a watermark to the PDF file at point.
The user is asked to provide a PDF file with the watermark and a
filename to save the new file to.

#### Print metadata

`pdftk-dired-metadata` prints the metadata of the PDF file at point to
a temporary buffer.


## Contributing
Contributions are welcome! If you find any issues or have suggestions for improvements, please open an issue or submit a pull request.

## License

Free as defined in the [MIT](https://choosealicense.com/licenses/mit/)
license.

## Acknowledgments
Arthur Miller for writing the dired-queue package, which is used to
queue the PDF files for merging.
