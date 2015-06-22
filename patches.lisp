;; Patches for external libraries.

(in-package #:lbot)

(defun rss::setf-unique-string (object name node)
  "Set the slot with the given NAME in OBJECT to the string contents of NODE,
throwing an error if it was already set, or if they aren't a string. Used for
elements like <title> and <link>, which shouldn't crop up twice."
  (let ((string (car (xmls:xmlrep-children node))))
    ;; skip empty nodes
    (when string
      (progn
        (unless (stringp string)
          (error 'rss-parse-error
                 :msg (format nil "Got ~A when expecting string for contents of <~A>"
                              string name)))
        (rss::setf-unique-slot object name string)))))

(defmacro rss::def-child-parser (name (&rest unique-strings)
                                &rest complicated-forms)
  "Define a parser that sets UNIQUE-STRINGS in the obvious
way. COMPLICATED-FORMS should be lists (KEY &body BODY) where KEY is a string
and BODY is performed with ITEM set to the item we're modifying and NODE set to
the XML node we just got."
  `(defun ,name (node object strict?)
     (declare (ignorable strict?))
     ;; skip atom-related tags
     (when (equalp "http://www.w3.org/2005/Atom" (xmls:node-ns node))
       (return-from ,name nil))
     (string=-case (xmls:xmlrep-tag node)
         (,@(mapcar
             (lambda (sym) `(,(rss::symbol-to-name sym)
                         (setf-unique-string object ',sym node)))
             unique-strings)
            ,@complicated-forms))))

(ql:quickload "rss")
