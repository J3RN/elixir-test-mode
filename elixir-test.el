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
    map)
  "Keymap to be invoked post-prefix.")
(fset 'elixir-test-command-map elixir-test-command-map)

(defvar elixir-test-mode-map (make-sparse-keymap)
  "An empty keymap primarily for setting the prefix.")

(defvar elixir-test-last-test nil
  "The last test run with an elixir-test-* function.")

(defun elixir-test-find-project-root ()
  "Traverse upwards from current buffer until a mix.exs file is discovered."
  (locate-dominating-file buffer-file-name "mix.exs"))

(defun elixir-test-format-command (cmd)
  "Formats CMD to be a command ready for `compile'."
  (string-join cmd " "))

(defun elixir-test-run-test (location)
  "Run the test specified by LOCATION.

LOCATION can be either a file and line, just a file, or a empty
string, specifying to run either a single test, a file, or the
whole suite, respectively."
  (let ((default-directory (elixir-test-find-project-root))
	(test-cmd
	 (cond
	  ((equal 'last location) elixir-test-last-test)
	  (current-prefix-arg (elixir-test-format-command
			       (list elixir-test-base-cmd
				     (read-from-minibuffer "flags: ")
				     location)))
	  (t (elixir-test-format-command (list elixir-test-base-cmd location))))))
    (setq elixir-test-last-test test-cmd)
    (compile test-cmd)))

(defun elixir-test-at-point ()
  "Run the test nearest to the point."
  (interactive)
  (let* ((line (line-number-at-pos (point)))
	 (file-and-line (format "%s:%s" buffer-file-name line)))
    (elixir-test-run-test file-and-line)))

(defun elixir-test-file ()
  "Test the current file."
  (interactive)
  (elixir-test-run-test buffer-file-name))

(defun elixir-test-directory ()
  "Test all files in the current directory."
  (interactive)
  (elixir-test-run-test default-directory))

(defun elixir-test-all ()
  "Test all files in the current test suite."
  (interactive)
  (elixir-test-run-test ""))

(defun elixir-test-rerun-last ()
  "Rerun whatever test was run last."
  (interactive)
  (if elixir-test-last-test (elixir-test-run-test 'last)
    (message "No test has been run yet!")))

;;;###autoload
(define-minor-mode elixir-test-mode
  "Minor mode to aid in testing Elixir code.

\\{elixir-test-mode-map}"
  :keymap elixir-test-mode-map)

;;;###autoload
(add-hook 'elixir-mode-hook 'elixir-test-mode)

(provide 'elixir-test)
;;; elixir-test.el ends here
