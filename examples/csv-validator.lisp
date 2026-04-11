;;;; csv-validator.lisp — CSV validator with telos intent and agent-aware restarts
;;;;
;;;; Companion code for "Conditions, Restarts, and the Agent That Chooses"
;;;; Based on Chaitanya Gupta's restarts tutorial: https://lisper.in/restarts
;;;;
;;;; Dependencies: telos, cl-ppcre
;;;;
;;;; Load:
;;;;   (ql:quickload :cl-ppcre)
;;;;   (asdf:load-system :telos)
;;;;   (load "examples/csv-validator.lisp")

(in-package :cl-user)

;;; ============================================================
;;; Features — the intent hierarchy
;;; ============================================================

(telos:deffeature csv-validation
  :purpose "Validate CSV files against header-defined field schemas with recoverable error handling"
  :goals ((:data-integrity "Every field value conforms to its header's type and format rules")
          (:graceful-recovery "Validation failures are recoverable at field, row, and file granularity")
          (:programmatic-collection "Callers can collect all errors without entering the debugger"))
  :constraints ((:single-pass "Validation processes the file sequentially, row by row")
                (:header-driven "The first row defines field names; validation rules are keyed to headers"))
  :assumptions ((:well-formed-csv "Input is comma-separated with consistent delimiters")
                (:known-headers "Headers map to a known set of validation rules"))
  :decisions ((:id :layered-restarts
               :chose "Three-level restart hierarchy: field, row, file"
               :over ("Single abort-all restart" "Per-field restarts only")
               :because "Field-level lets callers skip bad cells while validating remaining fields. Row-level lets callers skip structurally malformed rows. File-level lets callers fix the source and retry. Each level enables a different recovery strategy without dictating which one to use.")))

(telos:deffeature field-validation
  :purpose "Validate individual CSV field values against header-specific format rules"
  :belongs-to csv-validation
  :goals ((:field-correct "Each field value satisfies its header's validation predicate"))
  :failure-modes ((:unknown-header "Header name has no associated validation rule"
                   :violates :data-integrity)
                  (:format-mismatch "Field value does not match expected format"
                   :violates :field-correct)))

(telos:deffeature error-recovery
  :purpose "Provide structured restart points for non-local recovery from validation errors"
  :belongs-to csv-validation
  :goals ((:restart-availability "Every validation error has at least one applicable restart")
          (:caller-autonomy "The handler—human or agent—chooses recovery strategy, not the validator"))
  :assumptions ((:competent-handler "The handler has enough context to choose an appropriate restart")))

(telos:deffeature agent-recovery
  :purpose "Provide structured context for LLM agents to make informed restart decisions"
  :belongs-to csv-validation
  :goals ((:structured-context "Agent receives machine-readable recovery context, not just error strings")
          (:informed-choice "Agent can reason about goals, failure modes, and consequences before choosing"))
  :assumptions ((:llm-handler "The handler is an LLM-based agent capable of structured reasoning")
                (:restart-awareness "The agent understands that restarts represent non-local recovery options")))

;;; ============================================================
;;; Condition — the error type with intent
;;; ============================================================

(telos:define-condition/i csv-error (error)
  ((message
    :initarg :message
    :accessor csv-error-message
    :initform nil
    :documentation "Text message indicating what went wrong with the validation.")
   (value
    :initarg :value
    :accessor csv-error-value
    :initform nil
    :documentation "The value of the field for which the error is signalled.")
   (line-number
    :initarg :line-number
    :accessor csv-error-line-number
    :initform nil
    :documentation "The line number of the row for which the error was signalled."))
  (:feature csv-validation)
  (:role "Unified signal type for all validation failures, carrying enough context for recovery decisions")
  (:purpose "Bridge between low-level validation checks and high-level recovery handlers"))

(defmethod print-object ((object csv-error) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~@[L~A ~]~S~@[: ~S~]"
            (csv-error-line-number object)
            (csv-error-message object)
            (csv-error-value object))))

(defun csv-error (message &key value line-number)
  (error 'csv-error
         :message message
         :value value
         :line-number line-number))

;;; ============================================================
;;; Field validators — each with declared failure modes
;;; ============================================================

(telos:defun/i validate-url (string)
  "The URL of the page; should start with http:// or https://."
  (:feature field-validation)
  (:role "Enforce URL protocol prefix requirement")
  (:failure-modes ((:no-protocol "URL missing http:// or https:// prefix"
                    :violates :field-correct)))
  (unless (cl-ppcre:scan "^https?://" string)
    (csv-error "URL invalid." :value string)))

(telos:defun/i validate-rating (string)
  "String should contain an integer between 1 and 5, inclusive."
  (:feature field-validation)
  (:role "Enforce rating value is an integer within the valid range")
  (:failure-modes ((:invalid-rating "Rating is not an integer in range 1-5"
                    :violates :field-correct)))
  (let ((rating (parse-integer string :junk-allowed t)))
    (unless (and (integerp rating) (<= 1 rating 5))
      (csv-error "Rating not an integer in range." :value string))))

(telos:defun/i validate-visitors (string)
  "The number of visitors to the page; string should contain a non-negative integer."
  (:feature field-validation)
  (:role "Enforce visitor count is a non-negative integer")
  (:failure-modes ((:invalid-visitors "Visitor count is not a non-negative integer"
                    :violates :field-correct)))
  (let ((visitors (parse-integer string :junk-allowed t)))
    (unless (and (integerp visitors) (>= visitors 0))
      (csv-error "Number of visitors invalid." :value string))))

(telos:defun/i validate-date (string)
  "The published date of the URL. Should be in yyyy-mm-dd format."
  (:feature field-validation)
  (:role "Enforce date is in yyyy-mm-dd format with valid digit counts")
  (:failure-modes ((:invalid-date-format "Date not in yyyy-mm-dd format"
                    :violates :field-correct)))
  (let ((split (cl-ppcre:split "-" string)))
    (flet ((valid-number-of-digits-p (string n)
             (and (every #'digit-char-p string)
                  (= (length string) n))))
      (unless (and (= (length split) 3)
                   (valid-number-of-digits-p (first split) 4)
                   (valid-number-of-digits-p (second split) 2)
                   (valid-number-of-digits-p (third split) 2))
        (csv-error "Published date not in valid format." :value string)))))

(telos:defun/i validate-field (header value)
  "Dispatch field validation based on header name."
  (:feature field-validation)
  (:role "Route each field to the correct validator based on its column header")
  (flet ((header-matches (string)
           (string-equal header string)))
    (cond
      ((header-matches "url") (validate-url value))
      ((header-matches "rating") (validate-rating value))
      ((header-matches "visitors") (validate-visitors value))
      ((header-matches "date") (validate-date value))
      (t (csv-error "Invalid header." :value header)))))

;;; ============================================================
;;; Parser and validator — restarts at every level
;;; ============================================================

(telos:defun/i parse-csv-file (file)
  "Parse a CSV file into a list of rows, each row a list of field strings."
  (:feature csv-validation)
  (:role "Convert raw CSV text into structured row/field lists for validation")
  (:assumptions ((:comma-delimited "Fields are separated by commas with no quoting")))
  (with-open-file (f file :direction :input)
    (loop for line = (read-line f nil)
          while line
          collect (cl-ppcre:split "," line))))

(telos:defun/i validate-csv (file)
  "Validate a CSV file, establishing the file-level retry restart."
  (:feature error-recovery)
  (:role "Top-level entry point that establishes the file-retry restart")
  (restart-case (validate-csv-aux file)
    (retry-file ()
      :report (lambda (stream)
                (format stream "Retry validating the file ~A." file))
      (validate-csv file))))

(telos:defun/i validate-csv-aux (file)
  "Walk rows and fields, establishing row-level and field-level restarts."
  (:feature error-recovery)
  (:role "Establish per-row and per-field restarts around validation dispatch")
  (destructuring-bind (headers . rows)
      (parse-csv-file file)
    (loop for row in rows
          for line-number upfrom 2
          do (with-simple-restart (continue-next-row
                                   "Continue validation on next row.")
               (when (/= (length row) (length headers))
                 (csv-error "Number of fields doesn't equal number of headers."
                            :line-number line-number))
               (loop for header in headers
                     for field in row
                     do (handler-bind
                            ((csv-error
                              (lambda (c)
                                (setf (csv-error-line-number c) line-number))))
                          (with-simple-restart (continue-next-field
                                               "Continue validation on next field.")
                            (validate-field header field))))))))

;;; ============================================================
;;; Error collector — programmatic restart invocation
;;; ============================================================

(telos:defun/i list-csv-errors (file)
  "Collect all validation errors from a CSV file without entering the debugger."
  (:feature error-recovery)
  (:role "Programmatic handler that invokes restarts to collect all errors in one pass")
  (let ((result nil))
    (handler-bind
        ((csv-error (lambda (c)
                      (let ((restart (or (find-restart 'continue-next-field)
                                         (find-restart 'continue-next-row))))
                        (when restart
                          (push c result)
                          (invoke-restart restart))))))
      (validate-csv file))
    (nreverse result)))

;;; ============================================================
;;; Agent-facing API — structured context for LLM handlers
;;; ============================================================

(telos:defun/i build-recovery-context (condition)
  "Build structured context an LLM agent needs to choose a restart.

   Returns a plist with:
   - :error       — what went wrong (message, value, line)
   - :goals       — what the system is trying to achieve
   - :restarts    — available recovery options with descriptions
   - :design-rationale — why the restart architecture exists"
  (:feature agent-recovery)
  (:role "Transform runtime condition + telos metadata into agent-legible recovery context")
  (let* ((restarts (compute-restarts condition))
         (feature-intent (telos:feature-intent 'csv-validation))
         (goals (telos:intent-goals feature-intent))
         (decisions (telos:list-decisions 'csv-validation)))
    (list
     :error (list :message (csv-error-message condition)
                  :value (csv-error-value condition)
                  :line (csv-error-line-number condition))
     :goals goals
     :available-restarts
     (loop for r in restarts
           for name = (restart-name r)
           when (member name '(continue-next-field continue-next-row retry-file))
           collect (list :name name
                         :description (with-output-to-string (s)
                                        (princ r s))))
     :design-rationale
     (mapcar (lambda (d)
               (list :chose (telos:decision-chose d)
                     :over (telos:decision-over d)
                     :because (telos:decision-because d)))
             decisions))))

(telos:defun/i agent-handle-csv-error (condition &key (strategy :collect-all))
  "An agent-style handler that uses telos metadata to choose restarts.

   STRATEGY determines the agent's recovery policy:
   - :collect-all  — skip every bad field, gather all errors (audit mode)
   - :strict-rows  — skip the entire row on first field error (pipeline mode)
   - :fix-and-retry — log problems for agent reasoning (interactive mode)"
  (:feature agent-recovery)
  (:role "Demonstrate agent-as-handler: choose restarts based on structured intent context")
  (:goals ((:informed-decision "Agent selects restart based on system goals, not string matching")))
  (let ((context (build-recovery-context condition)))
    (ecase strategy
      ;; Audit mode: skip individual fields, keep validating everything.
      ;; Serves goal :PROGRAMMATIC-COLLECTION.
      (:collect-all
       (let ((restart (find-restart 'continue-next-field)))
         (when restart (invoke-restart restart))))

      ;; Pipeline mode: one bad field taints the whole row.
      ;; Serves goal :DATA-INTEGRITY (strict interpretation).
      (:strict-rows
       (let ((restart (find-restart 'continue-next-row)))
         (when restart (invoke-restart restart))))

      ;; Interactive mode: log what the agent sees, then skip field.
      ;; In a real system, the LLM would reason over the context here.
      ;; Serves goal :GRACEFUL-RECOVERY.
      (:fix-and-retry
       (let ((err (getf context :error))
             (restarts (getf context :available-restarts)))
         (format t "~&[Agent] Line ~A: ~A (value: ~S)~%"
                 (getf err :line)
                 (getf err :message)
                 (getf err :value))
         (format t "~&[Agent] Restarts: ~{~A~^, ~}~%"
                 (mapcar (lambda (r) (getf r :name)) restarts)))
       (let ((restart (find-restart 'continue-next-field)))
         (when restart (invoke-restart restart)))))))
