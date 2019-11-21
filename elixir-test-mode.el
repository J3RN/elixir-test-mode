;;; elixir-test-mode.el --- Emacs plugin to test Elixir code

;; Copyright 2019 Jonathan Arnett

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

;; Provides a set of functions for testing Elixir code.  The functions
;; themselves are immediately available, but it is recommended to set
;; a prefix keyinding in order to take advantage of the keymap.
;; e.g.
;; (define-key elixir-test-mode-map (kbd "C-c e") 'elixir-test-command-map)

;;; Code:


;;; Customization
(defgroup elixir-test nil
  "Elixir testing functionality."
  :prefix "elixir-test-"
  :group 'languages)

(defcustom elixir-test-base-cmd "mix test"
  "The base command to be used when running Elixir tests."
  :type 'string
  :risky t
  :group 'elixir-test)

(defcustom elixir-test-prefer-umbrella t
  "Whether or not to prefer running tests from an umbrella, if it exists.

If there is no umbrella project, the value of this variable is irrelevant."
  :type 'boolean
  :group 'elixir-test)


;;; elixir-test-mode definition and configuration
(defvar elixir-test-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "s") #'elixir-test-at-point)
    (define-key map (kbd "f") #'elixir-test-file)
    (define-key map (kbd "d") #'elixir-test-directory)
    (define-key map (kbd "a") #'elixir-test-all)
    (define-key map (kbd "l") #'elixir-test-rerun-last)
    (define-key map (kbd "u") #'elixir-test-up)
    (define-key map (kbd ".") #'elixir-test-failed)
    map)
  "Keymap to be invoked post-prefix.")
(fset 'elixir-test-command-map elixir-test-command-map)

(defvar elixir-test-mode-map (make-sparse-keymap)
  "An empty keymap primarily for setting the prefix.")

(defvar elixir-test-last-test-table (make-hash-table :test 'equal)
  "A hash-table mapping projects to the last test run with an elixir-test-* function inside them.")

;;;###autoload
(define-minor-mode elixir-test-mode
  "Minor mode to aid in testing Elixir code.

\\{elixir-test-mode-map}"
  :keymap elixir-test-mode-map)

;;;###autoload
(add-hook 'elixir-mode-hook 'elixir-test-mode)


;;; elixir-test-output-mode definition and configuration
;;;###autoload
(defvar elixir-test-output-mode-map elixir-test-command-map
  "A keymap for the elixir-test-output buffer.")

(define-derived-mode elixir-test-output-mode compilation-mode "Elixir Test")

(derived-mode-set-keymap 'elixir-test-output-mode)

(add-to-list 'compilation-error-regexp-alist-alist '(elixir "\\([^ ]+\\.exs?\\):\\([0-9]+\\)" 1 2))
(add-to-list 'compilation-error-regexp-alist 'elixir)


;;; Private functions
(defun elixir-test--get-last-test (dir)
  "Get the last test run with elixir-test in DIR."
  (gethash dir elixir-test-last-test-table))

(defun elixir-test--set-last-test (dir cmd)
  "Set CMD as the last test run with elixir-test in DIR."
  (puthash dir cmd elixir-test-last-test-table))

(defun elixir-test--find-umbrella-root (start-dir)
  "Traverse upwards from START-DIR until highest mix.exs file is discovered."
  (when-let ((project-dir (locate-dominating-file start-dir "mix.exs")))
    (or (elixir-test--find-umbrella-root (elixir-test--up-directory project-dir))
	project-dir)))

(defun elixir-test--find-project-root ()
  "Traverse upwards from current buffer until a mix.exs file is discovered."
  (if elixir-test-prefer-umbrella
      (elixir-test--find-umbrella-root default-directory)
    (locate-dominating-file default-directory "mix.exs")))

(defun elixir-test--project-name (root-directory)
  "Return the project name for the project located at ROOT-DIRECTORY."
  (file-name-base (directory-file-name root-directory)))

(defun elixir-test-output--buffer-name (mode)
  "Create the name for the elixir-test-output buffer."
  (concat "*" mode " " (elixir-test--project-name default-directory) "*"))

(defun elixir-test--format-command (cmd)
  "Formats CMD to be a command ready for `compile'."
  (string-join (delete nil cmd) " "))

(defun elixir-test--up-directory (dir)
  "Return the directory above DIR."
  (file-name-directory (directory-file-name dir)))

(defun elixir-test--test-file-name (file-name)
  "Guess the name of the test file for FILE-NAME."
  (let* ((test-file-name (concat (file-name-sans-extension (file-name-nondirectory file-name)) "_test.exs"))
	 (test-directory (replace-regexp-in-string "/lib/" "/test/" (file-name-directory file-name))))
    (concat test-directory test-file-name)))

(defun elixir-test--run-test (cmd)
  "Run the test specified by CMD.

CMD is a vector in the format `[base-cmd args location]' where
BASE-CMD is the base testing command (e.g. `mix test'), ARGS is a
string containing additional arguments to be given to BASE-CMD,
and LOCATION is either a file name and line number, just a file
name, or nil, conveying the intent to run a single test, a test
file, or the whole test suite, respectively."
  (let* ((default-directory (elixir-test--find-project-root))
	 (base-cmd (or (elt cmd 0) elixir-test-base-cmd))
	 (args (if current-prefix-arg
		   (read-from-minibuffer "Args: " (elt cmd 1) nil nil 'elixir-test-args)
		 (elt cmd 1)))
	 (location (elt cmd 2))
	 (location (when location
		     (file-relative-name
		      (if current-prefix-arg
			  (read-file-name "Path: " location)
			location))))
	 (test-cmd (vector base-cmd args location)))
	  (elixir-test--set-last-test default-directory test-cmd)
	  (compilation-start (elixir-test--format-command test-cmd)
			     'elixir-test-output-mode
			     'elixir-test-output--buffer-name)))


;;; Public functions
(defun elixir-test-at-point ()
  "Run the test nearest to the point."
  (interactive)
  (let* ((line (line-number-at-pos (point)))
	 (file-and-line (format "%s:%s" buffer-file-name line)))
    (elixir-test--run-test (vector nil nil file-and-line))))

(defun elixir-test-file ()
  "Test the current file or the file associated with the current file."
  (interactive)
  (if (string-match-p "test\\.exs" buffer-file-name)
      (elixir-test--run-test (vector nil nil buffer-file-name))
    (elixir-test--run-test (vector nil nil (elixir-test--test-file-name buffer-file-name)))))

(defun elixir-test-directory ()
  "Test all files in the current directory."
  (interactive)
  (elixir-test--run-test (vector nil nil default-directory)))

(defun elixir-test-all ()
  "Test all files in the current test suite."
  (interactive)
  (elixir-test--run-test (vector nil nil nil)))

(defun elixir-test-rerun-last ()
  "Rerun whatever test was run last."
  (interactive)
  (let* ((root (elixir-test--find-project-root))
	 (last-cmd (elixir-test--get-last-test root)))
    (if last-cmd
	(elixir-test--run-test last-cmd)
      (message "No test has been run in the project yet!"))))

(defun elixir-test-up ()
  "Rerun the last test command, but in the next highest directory from the last run."
  (interactive)
  (let* ((root (elixir-test--find-project-root))
	 (last-cmd (elixir-test--get-last-test root)))
    (if last-cmd
	(seq-let [base-cmd args last-file] last-cmd
	  (let ((new-file (when last-file
			    (elixir-test--up-directory last-file))))
	    (elixir-test--run-test (vector base-cmd args new-file))))
      (message "No test has been run in the project yet!"))))

(defun elixir-test-failed ()
  "Run only the tests that failed in the last run."
  (interactive)
  (elixir-test--run-test (vector elixir-test-base-cmd "--failed" nil)))

(provide 'elixir-test)
;;; elixir-test-mode.el ends here
