;;; compilation-history-view.el --- View compilation history -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Jonathan Otsuka

;;; Commentary:
;; Paginated vtable-based UI for browsing compilation history records.

;;; Code:

(require 'cl-lib)
(require 'vtable)
(require 'compilation-history)

;;; Pagination

(cl-defstruct compilation-history-view-pagination
  "Pagination state for the compilation history view."
  (current-page 1)
  (total-records 0)
  (page-size 25))

(defun compilation-history-view--total-pages (pagination)
  "Return total number of pages for PAGINATION."
  (max 1 (ceiling (compilation-history-view-pagination-total-records pagination)
                   (compilation-history-view-pagination-page-size pagination))))

(defun compilation-history-view--page-offset (pagination)
  "Return the SQL OFFSET for the current page of PAGINATION."
  (* (1- (compilation-history-view-pagination-current-page pagination))
     (compilation-history-view-pagination-page-size pagination)))

(defun compilation-history-view--calculate-page-size ()
  "Calculate page size from current window height.
Subtracts space for header-line and pagination controls."
  (max 1 (- (window-height) 4)))

;;; Data Model

(defun compilation-history-view--format-duration (seconds)
  "Format SECONDS as a human-readable duration string.
Returns empty string if SECONDS is nil."
  (if seconds
      (format "%.1fs" seconds)
    ""))

(defun compilation-history-view--derive-status (exit-code killed end-time)
  "Derive status string from EXIT-CODE, KILLED flag, and END-TIME."
  (cond
   ((null end-time) "running")
   ((and killed (not (zerop killed))) "killed")
   ((and exit-code (zerop exit-code)) "success")
   (t "failure")))

(defun compilation-history-view--row-to-plist (row &optional index)
  "Convert a database ROW (list) to a plist for vtable display.
ROW is a list matching the SELECT * column order of the compilations table,
with an appended duration_seconds column from the SQL query.
INDEX is the 0-based row position within the current page."
  ;; Column order: id(0), buffer_name(1), compile_command(2), default_directory(3),
  ;; start_time(4), end_time(5), exit_code(6), killed(7), git_repo(8),
  ;; git_branch(9), git_commit(10), git_commit_message(11), git_remote_urls(12),
  ;; os(13), os_version(14), emacs_version(15), output(16), duration_seconds(17)
  (let* ((id (nth 0 row))
         (buffer-name (nth 1 row))
         (compile-command (nth 2 row))
         (default-directory (nth 3 row))
         (start-time (nth 4 row))
         (end-time (nth 5 row))
         (exit-code (nth 6 row))
         (killed (nth 7 row))
         (git-branch (nth 9 row))
         (git-commit (nth 10 row))
         (duration (nth 17 row)))
    (list :id id
          :buffer-name buffer-name
          :command compile-command
          :directory default-directory
          :start-time start-time
          :end-time end-time
          :duration duration
          :exit-code exit-code
          :status (compilation-history-view--derive-status exit-code killed end-time)
          :branch git-branch
          :commit git-commit
          :row-index (or index 0))))

(provide 'compilation-history-view)
;;; compilation-history-view.el ends here
