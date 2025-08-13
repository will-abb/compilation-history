;;; test-compilation-history.el --- Tests for compilation-history -*- lexical-binding: t; -*-

(require 'ert)
(require 'compilation-history)

(ert-deftest compilation-history--identifier-test ()
  (should (string-match-p
           (rx (repeat 8 num) "T" (repeat 12 num))
           (compilation-history--get-timestamp))))

(ert-deftest compilation-history--path-test ()
  (cl-letf (((symbol-function 'compilation-history--get-project-root)
             (lambda (dir) "/path/to/my-project")))
    ;; Test case 1: Compilation at project root
    (should (string=
             "my-project"
             (compilation-history--get-path-string "/path/to/my-project")))

    ;; Test case 2: Compilation in a subdirectory
    (should (string=
             "my-project--src--app"
             (compilation-history--get-path-string "/path/to/my-project/src/app")))))

(ert-deftest compilation-history--path-test-no-project-root ()
  (cl-letf (((symbol-function 'compilation-history--get-project-root)
             (lambda (dir) nil)))
    (should (string=
             "my-project"
             (compilation-history--get-path-string "/path/to/my-project")))))

(ert-deftest compilation-history--command-test ()
  (let ((compilation-history-command-truncate-length 25))
    ;; Test case 1: Command with special characters
    (should (string=
             "a-b-c-d-e"
             (compilation-history--sanitize-command "a!b@c#d$e")))

    ;; Test case 2: Command with consecutive hyphens
    (should (string=
             "a-b-c"
             (compilation-history--sanitize-command "a---b-c")))

    ;; Test case 3: Command truncation
    (should (string=
             "1234567890123456789012345"
             (compilation-history--sanitize-command "123456789012345678901234567890")))))

;; We will keep this test for the final integrated function
(ert-deftest compilation-history--generate-buffer-name-test ()
  (let ((compilation-history-command-truncate-length 25))
    (cl-letf (((symbol-function 'compilation-history--get-project-root)
               (lambda (dir) "/path/to/my-project")))
      (should (string=
               "*compilation-history-20250812T153000000000==my-project__make-all*"
               (compilation-history--generate-buffer-name "make all" "/path/to/my-project" "20250812T153000000000"))))))