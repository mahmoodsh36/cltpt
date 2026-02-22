(defpackage :cltpt/tests/commandline
  (:use :cl :fiveam)
  (:import-from :cltpt/tests :string=+diff))

(in-package :cltpt/tests/commandline)

(def-suite commandline-suite
  :description "tests for the command-line interface."
  :in cltpt/tests::cltpt-suite)

(in-suite commandline-suite)

(defun run-cltpt-command (&rest args)
  "run ./run.sh with ARGS and return stdout as a string. stderr is discarded."
  (let ((cmd (list* "env" "-u" "CL_SOURCE_REGISTRY" "-u" "ASDF_OUTPUT_TRANSLATIONS" "./run.sh" args)))
    (multiple-value-bind (stdout stderr exit-code)
        (uiop:run-program cmd
                          :output '(:string)
                          :error-output :string
                          :ignore-error-status t)
      stdout)))

(test agenda-with-date-range
  "test the agenda command with --from and --to arguments."
  (let ((output (run-cltpt-command "agenda"
                                   "--input" "tests/test.org"
                                   "--from" "2025-07-25"
                                   "--to" "2025-08-01")))
    (is (string=+diff
         output
         "├─ Friday 25 July 2025
│ ├─ 00:00
│ ├─ 02:00
│ ├─ 04:00
│ ├─ 06:00
│ ├─ 08:00
│ ├─ 10:00
│ ├─ 12:00
│ ├─ 14:00
│ ├─ 16:00
│ ├─ 18:00
│ ├─ 20:00
│ └─ 22:00
├─ Saturday 26 July 2025
│ └─ START: (TODO) 10:55 header my secondary header     :tag1:tag2:important:
├─ Sunday 27 July 2025
│ ├─ START: (TODO) 10:55 header do something                       :noexport:
│ └─ START: (TODO) 17:55 do something else
├─ Monday 28 July 2025
├─ Tuesday 29 July 2025
│ └─ TODO (10:00) repeating task with last repeat                      :test:
├─ Wednesday 30 July 2025
│ ├─ TODO (10:00) repeating task with last repeat                      :test:
│ └─ DEADLINE: TODO 10:00 send the professor a mail
└─ Thursday 31 July 2025
  └─ TODO (10:00) repeating task with last repeat                      :test:
"
         "agenda commandline test")
        "agenda commandline test")))