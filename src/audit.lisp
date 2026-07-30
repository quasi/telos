(in-package :telos)

;;; Intent reference audit
;;;
;;; Some mistakes are local to one declaration — an unknown key, a wrong value
;;; type — and those are rejected at macroexpansion time. Others are topology:
;;; a :violates naming a goal that does not exist, a :belongs-to naming a feature
;;; that was never defined, a cycle in the hierarchy. Those cannot be judged as a
;;; declaration is read, because the feature it refers to may not be defined yet
;;; and load order is arbitrary; a member legitimately violates a goal declared
;;; on its parent.
;;;
;;; So: local shape is strict at macroexpansion, cross-declaration topology is
;;; reported here, over the finished image, on demand. This never signals — a
;;; partial or in-progress load shows transient dangling references, and that is
;;; not an error, it is a load in progress.

(defun ancestor-features (feature-name)
  "Feature names from FEATURE-NAME up to its root, FEATURE-NAME first.
   Stops on a repeated name, so a cyclic hierarchy is a finding, not a hang."
  (loop with seen = nil
        for name = feature-name then (intent-belongs-to intent)
        while (and name (not (member name seen)))
        for intent = (feature-intent name)
        while intent
        do (push name seen)
        collect name))

(defun hierarchy-cycle-p (feature-name)
  "True if walking up from FEATURE-NAME returns to a feature already visited."
  (loop with seen = nil
        for name = feature-name then (intent-belongs-to intent)
        while name
        for intent = (feature-intent name)
        while intent
        do (when (member name seen)
             (return t))
           (push name seen)
        finally (return nil)))

(defun reachable-goal-ids (intent)
  "Goal ids INTENT may refer to: its own, plus those of every ancestor feature.
   A member's failure mode usually violates a goal declared on its feature — that
   is the documented pattern, not an exception."
  (let ((ids (mapcar #'intent-entry-id (intent-goals intent))))
    (dolist (feature (ancestor-features (intent-belongs-to intent)) ids)
      (let ((feature-intent (feature-intent feature)))
        (when feature-intent
          (setf ids (append ids (mapcar #'intent-entry-id (intent-goals feature-intent)))))))))

(defun intent-finding (severity code entity entity-type reference message)
  (list :severity severity
        :code code
        :entity entity
        :entity-type entity-type
        :reference reference
        :message message))

(defun check-one-intent (entity entity-type intent)
  "Findings for a single INTENT: dangling :violates, undefined or cyclic parent."
  (let ((findings nil)
        (parent (intent-belongs-to intent))
        (reachable (reachable-goal-ids intent)))
    (when (and parent (null (feature-intent parent)))
      (push (intent-finding
             :error :undefined-parent entity entity-type parent
             (format nil "~S belongs to ~S, which is not a defined feature."
                     entity parent))
            findings))
    (when (and parent (hierarchy-cycle-p parent))
      (push (intent-finding
             :error :cyclic-hierarchy entity entity-type parent
             (format nil "The :BELONGS-TO chain from ~S is cyclic." entity))
            findings))
    (dolist (mode (intent-failure-modes intent))
      (let ((violates (intent-entry-option mode :violates)))
        (when (and violates (not (member violates reachable)))
          (push (intent-finding
                 :error :dangling-violates entity entity-type violates
                 (format nil "Failure mode ~S of ~S violates ~S, which is not a goal ~
                              of ~:*~*~S or of any feature it belongs to."
                         (intent-entry-id mode) entity violates entity))
                findings))))
    findings))

(defun all-intents ()
  "Every intent in the image as (entity entity-type intent).

   Registries are candidate indexes, never truth: a class defined with
   DEFCLASS/I keeps its intent on the metaobject, so it is re-derived here rather
   than read out of a table that nothing updates."
  (let ((intents nil))
    (loop for name being the hash-keys of *feature-registry*
            using (hash-value intent)
          do (push (list name :feature intent) intents))
    (loop for key being the hash-keys of *entity-intent-registry*
            using (hash-value intent)
          do (destructuring-bind (kind name) key
               (push (list name kind intent) intents)))
    (loop for name being the hash-keys of *class-intent-registry*
            using (hash-value intent)
          do (push (list name :class intent) intents))
    (loop for spec being the hash-keys of *method-intent-registry*
            using (hash-value intent)
          do (push (list spec :method intent) intents))
    (dolist (name (all-intentful-classes))
      (let ((intent (class-intent name)))
        (when intent
          (push (list name :class intent) intents))))
    intents))

(defun finding-sort-key (finding)
  (format nil "~A|~A|~A"
          (getf finding :code) (getf finding :entity) (getf finding :reference)))

(defun check-intent-references ()
  "Report cross-declaration mistakes in the intent graph. Returns a list of
   findings, most predictable order first, or NIL when everything resolves.

   Each finding is a plist: :SEVERITY, :CODE, :ENTITY, :ENTITY-TYPE, :REFERENCE,
   :MESSAGE. The codes are API — dispatch on them:

     :DANGLING-VIOLATES  a failure mode violates a goal that does not exist here
                         or on any feature it belongs to
     :UNDEFINED-PARENT   :BELONGS-TO names a feature that was never defined
     :CYCLIC-HIERARCHY   the :BELONGS-TO chain loops

   Never signals: run it whenever you like, including mid-load, where a dangling
   reference means only that the rest has not been loaded yet. Use
   ASSERT-INTENT-REFERENCES in a test if you want it to fail loudly."
  (let ((findings (loop for (entity entity-type intent) in (all-intents)
                        append (check-one-intent entity entity-type intent))))
    ;; Registry iteration is hash-ordered; a report a human or CI diffs must not be.
    (sort findings #'string< :key #'finding-sort-key)))

(define-condition intent-reference-error (error)
  ((findings :initarg :findings :initform nil
             :reader intent-reference-error-findings
             :documentation "The findings from CHECK-INTENT-REFERENCES."))
  (:documentation "Signalled by ASSERT-INTENT-REFERENCES when the intent graph has
unresolved references. Carries the findings so a handler can report them.")
  (:report (lambda (condition stream)
             (let ((findings (intent-reference-error-findings condition)))
               (format stream "~D unresolved intent reference~:P:~{~%  ~A~}"
                       (length findings)
                       (mapcar (lambda (f) (getf f :message)) findings))))))

(defun assert-intent-references ()
  "Signal INTENT-REFERENCE-ERROR if CHECK-INTENT-REFERENCES finds anything.
   One line for a test or a CI step; returns NIL when the graph is clean."
  (let ((findings (check-intent-references)))
    (when findings
      (error 'intent-reference-error :findings findings))
    nil))
