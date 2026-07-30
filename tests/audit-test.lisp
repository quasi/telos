(in-package :telos/tests)

(def-suite audit-tests :in :telos-tests)
(in-suite audit-tests)

;;; Enumerating intentful classes
;;;
;;; defclass/i stores intent on the class metaobject, so a class with no :feature
;;; appears in no registry at all — invisible to anything that sweeps the image.

(test all-intentful-classes-finds-a-featureless-class
  "A defclass/i with no :feature is in no registry, but must still be enumerable"
  (eval '(defclass/i audit-bare-class () () (:purpose "No feature at all")))
  (is (member 'audit-bare-class (all-intentful-classes)))
  ;; the premise: it really is in neither registry
  (is (null (gethash 'audit-bare-class telos::*class-intent-registry*)))
  (is (not (member 'audit-bare-class
                   (loop for key being the hash-keys of telos::*entity-intent-registry*
                         collect (second key))))))

(test all-intentful-classes-finds-a-featured-class
  (eval '(deffeature audit-class-feature :purpose "Owner"))
  (eval '(defclass/i audit-featured-class ()
          ()
          (:feature audit-class-feature)
          (:purpose "Has a feature")))
  (is (member 'audit-featured-class (all-intentful-classes))))

(test all-intentful-classes-is-an-index-not-truth
  "A name in the index whose class is not intentful drops out on read, rather than
   producing a phantom finding.

   CLOS forbids changing a class's metaclass, so a defclass/i class cannot become
   an ordinary one; staleness instead looks like a name that never became an
   intentful class in the first place."
  (eval '(defclass audit-plain-class () ()))
  (setf (gethash 'audit-plain-class telos::*intentful-class-names*) t)
  (is (not (member 'audit-plain-class (all-intentful-classes)))))

(test all-intentful-classes-tolerates-an-uninterned-name
  "A name that no longer names a class must not break enumeration"
  (setf (gethash 'audit-never-defined-class telos::*intentful-class-names*) t)
  (finishes (all-intentful-classes))
  (is (not (member 'audit-never-defined-class (all-intentful-classes)))))

;;; feature-members must not report a member that has moved away
;;;
;;; register-member only ever pushes, so re-declaring an entity under a second
;;; feature used to leave it listed under both.

(test feature-members-drops-a-member-that-moved
  (eval '(deffeature audit-home-a :purpose "First home"))
  (eval '(deffeature audit-home-b :purpose "Second home"))
  (eval '(defun/i audit-moving-fn () (:feature audit-home-a) (:purpose "Here first") nil))
  (is (member 'audit-moving-fn (feature-members 'audit-home-a :functions)))
  (eval '(defun/i audit-moving-fn () (:feature audit-home-b) (:purpose "Moved") nil))
  (is (not (member 'audit-moving-fn (feature-members 'audit-home-a :functions))))
  (is (member 'audit-moving-fn (feature-members 'audit-home-b :functions))))

(test feature-members-drops-a-class-that-moved
  (eval '(deffeature audit-chome-a :purpose "First home"))
  (eval '(deffeature audit-chome-b :purpose "Second home"))
  (eval '(defclass/i audit-moving-class () () (:feature audit-chome-a) (:purpose "Here first")))
  (is (member 'audit-moving-class (feature-members 'audit-chome-a :classes)))
  (eval '(defclass/i audit-moving-class () () (:feature audit-chome-b) (:purpose "Moved")))
  (is (not (member 'audit-moving-class (feature-members 'audit-chome-a :classes))))
  (is (member 'audit-moving-class (feature-members 'audit-chome-b :classes))))

(test feature-members-still-reports-members-that-stayed
  "The read-time check must not throw away legitimate members of every kind"
  (eval '(deffeature audit-stay-feature :purpose "Owner"))
  (eval '(defun/i audit-stay-fn () (:feature audit-stay-feature) (:purpose "p") nil))
  (eval '(defclass/i audit-stay-class () () (:feature audit-stay-feature) (:purpose "p")))
  (eval '(defstruct/i audit-stay-struct a (:feature audit-stay-feature) (:purpose "p")))
  (eval '(define-condition/i audit-stay-condition (error)
          ()
          (:feature audit-stay-feature) (:purpose "p")))
  (let ((members (feature-members 'audit-stay-feature)))
    (is (member 'audit-stay-fn (getf members :functions)))
    (is (member 'audit-stay-class (getf members :classes)))
    (is (member 'audit-stay-struct (getf members :structs)))
    (is (member 'audit-stay-condition (getf members :conditions)))))

;;; check-intent-references
;;;
;;; :violates names a goal declared here or on an ancestor feature. It cannot be
;;; resolved at macroexpansion time — the ancestor may not be defined yet — so it
;;; is resolved here, over the finished image.

(defun finding-for (findings code entity)
  "The first finding with CODE about ENTITY, or nil."
  (find-if (lambda (f)
             (and (eq code (getf f :code)) (equal entity (getf f :entity))))
           findings))

(test check-finds-a-dangling-violates
  (eval '(deffeature audit-dangler
          :purpose "Has a typo'd violates"
          :goals ((:real-goal "A goal that exists"))
          :failure-modes ((:fm1 "Points at nothing" :violates :no-such-goal))))
  (let ((finding (finding-for (check-intent-references) :dangling-violates 'audit-dangler)))
    (is (not (null finding)))
    (is (eq :error (getf finding :severity)))
    (is (eq :no-such-goal (getf finding :reference)))
    (is (search "NO-SUCH-GOAL" (getf finding :message)))))

(test check-accepts-a-violates-resolved-on-an-ancestor
  "The pattern the library's own example uses: a child violates its parent's goal"
  (eval '(deffeature audit-parent :purpose "Parent" :goals ((:parent-goal "Parent's goal"))))
  (eval '(deffeature audit-child
          :purpose "Child"
          :belongs-to audit-parent
          :goals ((:child-goal "Child's own goal"))
          :failure-modes ((:fm1 "Violates the parent's goal" :violates :parent-goal))))
  (is (null (finding-for (check-intent-references) :dangling-violates 'audit-child))))

(test check-resolves-through-a-grandparent
  "Resolution searches the whole ancestor chain, not just the first parent"
  (eval '(deffeature audit-grand :purpose "Grandparent" :goals ((:grand-goal "Far away"))))
  (eval '(deffeature audit-mid :purpose "Middle" :belongs-to audit-grand))
  (eval '(deffeature audit-leaf
          :purpose "Leaf"
          :belongs-to audit-mid
          :failure-modes ((:fm1 "Reaches up two levels" :violates :grand-goal))))
  (is (null (finding-for (check-intent-references) :dangling-violates 'audit-leaf))))

(test check-covers-members-not-just-features
  "A function's failure mode usually violates a goal declared on its feature"
  (eval '(deffeature audit-fn-feature :purpose "Owner" :goals ((:owner-goal "Owned"))))
  (eval '(defun/i audit-good-fn ()
          (:feature audit-fn-feature)
          (:purpose "p")
          (:failure-modes ((:fm1 "Fine" :violates :owner-goal)))
          nil))
  (eval '(defun/i audit-bad-fn ()
          (:feature audit-fn-feature)
          (:purpose "p")
          (:failure-modes ((:fm1 "Typo" :violates :owner-gaol)))
          nil))
  (let ((findings (check-intent-references)))
    (is (null (finding-for findings :dangling-violates 'audit-good-fn)))
    (is (not (null (finding-for findings :dangling-violates 'audit-bad-fn))))))

(test check-covers-a-featureless-intentful-class
  "The class that appears in no registry must still be audited"
  (eval '(defclass/i audit-lonely-class ()
          ()
          (:purpose "No feature, so no goals reachable")
          (:failure-modes ((:fm1 "Dangling" :violates :nowhere)))))
  (is (not (null (finding-for (check-intent-references)
                              :dangling-violates 'audit-lonely-class)))))

(test check-reports-an-undefined-parent
  (eval '(deffeature audit-orphan :purpose "Parent was never defined"
          :belongs-to audit-no-such-parent))
  (let ((finding (finding-for (check-intent-references) :undefined-parent 'audit-orphan)))
    (is (not (null finding)))
    (is (eq 'audit-no-such-parent (getf finding :reference)))))

(test check-reports-a-cycle-without-hanging
  (eval '(deffeature audit-cycle-a :purpose "A" :belongs-to audit-cycle-b))
  (eval '(deffeature audit-cycle-b :purpose "B" :belongs-to audit-cycle-a))
  (let ((findings (check-intent-references)))
    (is (not (null (finding-for findings :cyclic-hierarchy 'audit-cycle-a))))
    (is (not (null (finding-for findings :cyclic-hierarchy 'audit-cycle-b))))))

(test check-does-not-flag-a-goal-nothing-violates
  "A goal with no failure mode is normal, not a finding"
  (eval '(deffeature audit-unviolated :purpose "p" :goals ((:quiet-goal "Nobody violates me"))))
  (is (null (find-if (lambda (f) (equal 'audit-unviolated (getf f :entity)))
                     (check-intent-references)))))

(test check-output-is-deterministic
  "Registry iteration is hash-ordered; the report must not be"
  (is (equal (check-intent-references) (check-intent-references))))

(test check-findings-are-plists-with-a-stable-shape
  (let ((finding (first (check-intent-references))))
    (is (not (null finding)))
    (dolist (key '(:severity :code :entity :entity-type :reference :message))
      (is (member key finding)))))

(test assert-intent-references-signals-when-there-are-findings
  (eval '(deffeature audit-assert-dangler :purpose "p"
          :failure-modes ((:fm1 "d" :violates :not-a-goal))))
  (signals intent-reference-error (assert-intent-references)))

(test assert-intent-references-carries-the-findings
  (handler-case (progn (assert-intent-references) (fail "expected a signal"))
    (intent-reference-error (e)
      (is (listp (intent-reference-error-findings e)))
      (is (plusp (length (intent-reference-error-findings e)))))))
