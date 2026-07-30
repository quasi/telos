(in-package :telos)

;;; Declaration validation
;;;
;;; A declaration that silently loses a field is worse than no declaration at
;;; all: it answers queries as though the field had never been written. So every
;;; intent macro validates what it was given at macroexpansion time — both its
;;; top-level clauses and the nested plists (goals, failure modes, decisions) —
;;; and signals INVALID-INTENT-DECLARATION rather than dropping anything.

(defparameter *entry-shape-hint*
  " — the shape is (:id \"description\")"
  "Reminder of the documented entry shape, appended to shape-related messages.")

(defun report-invalid-intent-declaration (condition stream)
  (let* ((context (invalid-intent-declaration-context condition))
         (field (invalid-intent-declaration-field condition))
         (entry (invalid-intent-declaration-entry condition))
         (key (invalid-intent-declaration-key condition))
         (expected (invalid-intent-declaration-expected condition))
         (reason (invalid-intent-declaration-reason condition))
         (*print-pretty* nil))
    ;; Where the problem is.
    (case reason
      ((:unknown-clause :clause-arity)
       (format stream "In ~@[~A, ~]clause ~S: " context entry))
      ((:field-not-a-list :unevaluated-form)
       (format stream "In ~@[~A, ~]the value of ~S is ~S: " context field entry))
      (t
       (format stream "In ~@[~A, ~]~S entry ~S: " context field entry)))
    ;; What the problem is.
    (case reason
      (:unknown-key
       (format stream "unknown keyword: ~S; " key)
       (if expected
           (format stream "expected one of ~{~S~^, ~}" expected)
           (format stream "~S entries accept no keyword options~A" field *entry-shape-hint*)))
      (:unknown-clause
       (format stream "unknown intent clause: ~S; expected one of ~{~S~^, ~}" key expected))
      (:duplicate-key
       (format stream "duplicate keyword: ~S; the later value would silently replace the first"
               key))
      (:non-keyword-key
       (format stream "~S is not a keyword; ~S entries take keyword/value pairs~@[ (~{~S~^, ~})~]"
               key field expected))
      (:clause-arity
       (if (null (cdr entry))
           (format stream "a ~S clause needs a value" key)
           (format stream "a ~S clause takes exactly one value; everything after the first ~
                           would be dropped"
                   key)))
      (:options-before-description
       (format stream "~S sits where the description belongs, so the entry is ambiguous: ~S ~
                       reads as an option here and as the description to anything taking ~
                       (second entry). Write (:id \"description\" ~S ...) instead."
               key key key))
      (:unevaluated-form
       (format stream "field values are taken literally, never evaluated, so this would be ~
                       stored as written rather than producing a list of entries. Write the ~
                       entries out literally."))
      (:odd-plist
       (if expected
           (format stream "keyword options must come in pairs, one of ~{~S~^, ~}" expected)
           (format stream "~S entries accept no keyword options~A" field *entry-shape-hint*)))
      (:value-type
       (let ((value (getf entry key)))
         (format stream "~S must be ~A, not ~S" key expected value)
         ;; Only a compound value suggests someone expected evaluation.
         (when (consp value)
           (format stream ". Decision values are literal data; use RECORD-DECISION ~
                           for a computed decision"))))
      (:field-not-a-list
       (format stream "expected a list of entries~A; values are taken literally, not evaluated"
               *entry-shape-hint*))
      (:not-a-list
       (format stream "entry must be a proper list~A" *entry-shape-hint*))
      (t
       ;; Never fail while printing a condition.
       (format stream "invalid declaration (~S)~@[; offending key ~S~]" reason key)))))

(define-condition invalid-intent-declaration (program-error)
  ((context :initarg :context :initform nil
            :reader invalid-intent-declaration-context
            :documentation "Description of the declaration, e.g. \"DEFFEATURE FOO\".")
   (field :initarg :field :initform nil
          :reader invalid-intent-declaration-field
          :documentation "Which declaration field the bad entry came from, e.g. :GOALS.")
   (entry :initarg :entry :initform nil
          :reader invalid-intent-declaration-entry
          :documentation "The offending entry, as written.")
   (key :initarg :key :initform nil
        :reader invalid-intent-declaration-key
        :documentation "The offending keyword, when the problem is a key rather than a shape.")
   (expected :initarg :expected :initform nil
             :reader invalid-intent-declaration-expected
             :documentation "The keywords this kind of entry accepts.")
   (reason :initarg :reason :initform :unknown-key
           :reader invalid-intent-declaration-reason
           :documentation "One of :UNKNOWN-KEY, :UNKNOWN-CLAUSE, :CLAUSE-ARITY,
:DUPLICATE-KEY, :NON-KEYWORD-KEY, :OPTIONS-BEFORE-DESCRIPTION, :ODD-PLIST, :NOT-A-LIST,
:FIELD-NOT-A-LIST, :UNEVALUATED-FORM, :VALUE-TYPE."))
  (:documentation "Signalled at macroexpansion time when an intent declaration is
malformed or carries a key the declaration does not understand. A distinct type so
it can be grepped, handled, and escalated.")
  (:report report-invalid-intent-declaration))

(defun invalid-declaration (reason context field entry &key key expected)
  (error 'invalid-intent-declaration
         :reason reason :context context :field field :entry entry
         :key key :expected expected))

(defparameter *decision-keys*
  '(:id :chose :over :because :date :decided-by)
  "Keywords a decision plist accepts. Rationale goes in :BECAUSE.")

(defparameter *intent-entry-option-keys*
  '((:goals . nil)
    (:constraints . nil)
    (:assumptions . nil)
    (:verification . nil)
    (:failure-modes . (:violates)))
  "Keyword options each kind of intent entry accepts beyond (id \"description\").")

(defparameter *intent-clause-keys*
  '(:feature :role :purpose :failure-modes :goals :constraints :assumptions :verification)
  "Clause keywords the definition macros accept alongside a definition body.")

(defun proper-list-p (object)
  "True if OBJECT is a non-dotted list. Will not terminate on circular input;
   declarations come from source, so circularity is not a concern."
  (loop for tail = object then (cdr tail)
        until (atom tail)
        finally (return (null tail))))

(defun validate-plist-keys (plist expected context field entry)
  "Check that PLIST is a well-formed plist whose keys all appear in EXPECTED,
   with no key given twice."
  (when (oddp (length plist))
    (invalid-declaration :odd-plist context field entry :expected expected))
  (let ((seen nil))
    (loop for (key nil) on plist by #'cddr
          do (unless (keywordp key)
               (invalid-declaration :non-keyword-key context field entry
                                    :key key :expected expected))
             (unless (member key expected)
               (invalid-declaration :unknown-key context field entry
                                    :key key :expected expected))
             (when (member key seen)
               (invalid-declaration :duplicate-key context field entry
                                    :key key :expected expected))
             (push key seen))))

(defun entry-options (entry expected context field)
  "Return the keyword-option tail of ENTRY, whose shape is (id) or
   (id description . options). A keyword where the description belongs is the one
   genuinely ambiguous case — it reads as an option here and as the description to
   any consumer taking (second entry) — so it is rejected rather than guessed at.
   Other description values are odd but unambiguous, and not this validator's business."
  (when (keywordp (second entry))
    (invalid-declaration :options-before-description context field entry
                         :key (second entry) :expected expected))
  (cddr entry))

(defun form-valued-p (value)
  "True if VALUE looks like a form someone expects to be evaluated rather than
   literal data. A list of entries always has conses for elements, so a symbol in
   the head position is already wrong; asking whether that symbol names an operator
   just tells us which message to give. No allowlist of heads: (MAPCAR ...) and
   (REMOVE ...) are as much a mistake as (LIST ...)."
  (and (consp value)
       (car value)
       (symbolp (car value))
       (let ((head (car value)))
         (or (fboundp head)
             (macro-function head)
             (special-operator-p head)
             ;; Implementations name the quasiquote marker differently, and it is
             ;; not fbound.
             (string= (symbol-name head) "QUASIQUOTE")))))

(defun validate-intent-entries (field entries context)
  "Validate ENTRIES, the value of intent FIELD, e.g. :GOALS or :FAILURE-MODES.
   Each entry must be a list (id \"description\" . options), where options are
   keyword/value pairs drawn from the keys FIELD accepts."
  (let ((expected (cdr (assoc field *intent-entry-option-keys*))))
    (when (form-valued-p entries)
      (invalid-declaration :unevaluated-form context field entries :expected expected))
    (unless (proper-list-p entries)
      (invalid-declaration :field-not-a-list context field entries :expected expected))
    (dolist (entry entries)
      (unless (and (consp entry) (proper-list-p entry))
        (invalid-declaration :not-a-list context field entry :expected expected))
      (validate-plist-keys (entry-options entry expected context field)
                           expected context field entry))))

(defparameter *decision-value-types*
  '((:id keywordp "a keyword")
    (:chose stringp "a string")
    (:because stringp "a string")
    (:date stringp "a string")
    (:decided-by stringp "a string")
    (:over string-list-p "a list of strings"))
  "The type each decision field accepts, mirroring the DECISION struct's slot types.
   These are checked at macroexpansion time so the message names the field and the
   declaration, rather than surfacing as a MAKE-DECISION type error at load time.")

(defun string-list-p (value)
  "True if VALUE is a proper list of strings — the alternatives a decision rejected."
  (and (proper-list-p value)
       (every #'stringp value)))

(defun validate-decision-values (decision context)
  "Check each decision value against the type its field accepts. NIL is always
   allowed: every field but the id is optional."
  (loop for (key value) on decision by #'cddr
        for spec = (assoc key *decision-value-types*)
        do (destructuring-bind (&optional field predicate description) spec
             (declare (ignore field))
             (when (and predicate value (not (funcall predicate value)))
               (invalid-declaration :value-type context :decisions decision
                                    :key key :expected description)))))

(defun validate-decision-entries (decisions context)
  "Validate DECISIONS, a list of plists of the form
   (:id :x :chose \"A\" :over (\"B\") :because \"why\" :date \"...\" :decided-by \"...\")."
  (when (form-valued-p decisions)
    (invalid-declaration :unevaluated-form context :decisions decisions
                         :expected *decision-keys*))
  (unless (proper-list-p decisions)
    (invalid-declaration :field-not-a-list context :decisions decisions
                         :expected *decision-keys*))
  (dolist (decision decisions)
    (unless (and (consp decision) (proper-list-p decision))
      (invalid-declaration :not-a-list context :decisions decision
                           :expected *decision-keys*))
    (validate-plist-keys decision *decision-keys* context :decisions decision)
    (validate-decision-values decision context)))

(defun validate-intent-fields (plist context)
  "Validate every nested intent field present in PLIST (a make-intent-style plist).
   Unrecognized top-level keys are the caller's business — macros with a &key
   lambda list get that for free, the rest use VALIDATE-INTENT-CLAUSE."
  (loop for (field value) on plist by #'cddr
        do (when (assoc field *intent-entry-option-keys*)
             (validate-intent-entries field value context)))
  plist)

(defun invalid-intent-clause (clause context &optional (expected *intent-clause-keys*))
  "Signal that CLAUSE is not a recognized intent clause like (:role \"...\").
   Called by the macros that read clauses from a body or option list instead of a
   &key lambda list; without this an unknown clause is consumed and dropped — and in
   DEFUN/I it would take a body form with it."
  (invalid-declaration :unknown-clause context nil clause
                       :key (car clause) :expected expected))

(defun intent-clause-value (clause context)
  "Return the single value of intent CLAUSE, e.g. \"...\" from (:role \"...\").
   Anything beyond that one value would be dropped on the floor, so it is rejected:
   (:goals ((:g1 \"a\")) ((:g2 \"b\"))) is a mistake, not a two-argument clause."
  (unless (= 2 (length clause))
    (invalid-declaration :clause-arity context nil clause :key (car clause)))
  (second clause))

(defun declaration-context (macro-name name)
  "Format a context string like \"DEFFEATURE FOO\" for error messages."
  (format nil "~A ~S" macro-name name))
