;;; elixir-test.el --- Emacs plugin to test Elixir code

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

(defcustom elixir-test-base-cmd "mix test"
  "The base command to be used when running Elixir tests."
  :type 'string
  :risky t
  :group 'elixir-test)

(defvar elixir-test-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "s") #'elixir-test-at-point)
    (define-key map (kbd "f") #'elixir-test-file)
    (define-key map (kbd "d") #'elixir-test-directory)
    (define-key map (kbd "a") #'elixir-test-all)
    (define-key map (kbd "l") #'elixir-test-rerun-last)
    (define-key map (kbd "u") #'elixir-test-up)
    map)
  "Keymap to be invoked post-prefix.")
(fset 'elixir-test-command-map elixir-test-command-map)

(defvar elixir-test-mode-map (make-sparse-keymap)
  "An empty keymap primarily for setting the prefix.")

(defvar elixir-test-last-test-table (make-hash-table :test 'equal)
  "A hash-table mapping projects to the last test run with an elixir-test-* function inside them.")

(defun elixir-test-get-last-test (dir)
  "Get the last test run with elixir-test in DIR."
  (gethash dir elixir-test-last-test-table))

(defun elixir-test-set-last-test (dir cmd)
  "Set CMD as the last test run with elixir-test in DIR."
  (puthash dir cmd elixir-test-last-test-table))

(defun elixir-test-find-project-root ()
  "Traverse upwards from current buffer until a mix.exs file is discovered."
  (locate-dominating-file default-directory "mix.exs"))

(defun elixir-test-format-command (cmd)
  "Formats CMD to be a command ready for `compile'."
  (string-join (delete nil cmd) " "))

(defun elixir-test-run-test (cmd)
  "Run the test specified by CMD.

CMD is a vector in the format `[base-cmd args location]' where
BASE-CMD is the base testing command (e.g. `mix test'), ARGS is a
string containing additional arguments to be given to BASE-CMD,
and LOCATION is either a file name and line number, just a file
name, or nil, conveying the intent to run a single test, a test
file, or the whole test suite, respectively."
  (let* ((default-directory (elixir-test-find-project-root))
	 (base-cmd (or (elt cmd 0) elixir-test-base-cmd))
	 (args (if current-prefix-arg
		   (read-from-minibuffer "Args: " (elt cmd 1) nil nil 'elixir-test-args)
		 (elt cmd 1)))
	 (location (when (elt cmd 2) (file-relative-name (elt cmd 2))))
	 (test-cmd (vector base-cmd args location)))
    (elixir-test-set-last-test default-directory test-cmd)
    (compile (elixir-test-format-command test-cmd))))

(defun elixir-test-at-point ()
  "Run the test nearest to the point."
  (interactive)
  (let* ((line (line-number-at-pos (point)))
	 (file-and-line (format "%s:%s" buffer-file-name line)))
    (elixir-test-run-test (vector nil nil file-and-line))))

(defun elixir-test-file ()
  "Test the current file."
  (interactive)
  (elixir-test-run-test (vector nil nil buffer-file-name)))

(defun elixir-test-directory ()
  "Test all files in the current directory."
  (interactive)
  (elixir-test-run-test (vector nil nil default-directory)))

(defun elixir-test-all ()
  "Test all files in the current test suite."
  (interactive)
  (elixir-test-run-test (vector nil nil nil)))

(defun elixir-test-rerun-last ()
  "Rerun whatever test was run last."
  (interactive)
  (let* ((root (elixir-test-find-project-root))
	 (last-cmd (elixir-test-get-last-test root)))
    (if last-cmd
	(elixir-test-run-test last-cmd)
      (message "No test has been run in the project yet!"))))

(defun elixir-test--up-directory (dir)
  "Return the directory above DIR."
  (file-name-directory (directory-file-name dir)))

(defun elixir-test-up ()
  "Rerun the last test command, but in the next highest directory from the last run."
  (interactive)
  (let* ((root (elixir-test-find-project-root))
	 (last-cmd (elixir-test-get-last-test root)))
    (if last-cmd
	(seq-let [base-cmd args last-file] last-cmd
	  (let ((new-file (when last-file
			    (elixir-test--up-directory last-file))))
	    (elixir-test-run-test (vector base-cmd args new-file))))
      (message "No test has been run in the project yet!"))))

;;;###autoload
(define-minor-mode elixir-test-mode
  "Minor mode to aid in testing Elixir code.

\\{elixir-test-mode-map}"
  :keymap elixir-test-mode-map)

;;;###autoload
(add-hook 'elixir-mode-hook 'elixir-test-mode)

(provide 'elixir-test)
;;; elixir-test.el ends here
