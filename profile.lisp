(in-package :sample-profiler)

(defstruct profiler
  (process nil)
  (file nil)
  (last-result nil)
  output-file)

(defvar *profiler* (make-profiler))

(defvar *impurify-after-profile* nil)

(defun start-profiling (&key (seconds T) (verbose nil) (output-file "profile.html") (profiler *profiler*))
  (when (eq seconds T) (setq seconds (* 60 60 24)))
  (check-type seconds (integer (0)))
  (when (profiler-process profiler)
    (when (eq :running (ccl:external-process-status (profiler-process profiler)))
      (cerror "Abort existing profiler" "Profiling currently in effect: ~s." profiler))
    (end-profiling :abort t))
  (ccl::purify)
  (let* ((file (ccl:temp-pathname))
         (proc (ccl:run-program "/usr/bin/sample"
                                (list (princ-to-string (ccl::getpid))
                                      (princ-to-string seconds)
                                      "-file"
                                      (ccl:native-translated-namestring file))
                                :input nil
                                :wait nil
                                :output :stream)))
    (setf (profiler-output-file profiler) output-file)
    (setf (profiler-last-result profiler) nil)
    (setf (profiler-file profiler) file)
    (setf (profiler-process profiler) proc)
    ;; wait for some output to appear then copy it to standard output.
    (let ((stream (ccl:external-process-output-stream proc)))
      (when (peek-char nil stream nil)
        (when verbose (fresh-line))
        (loop as ch = (read-char-no-hang stream nil) while ch
           do (when verbose (write-char ch))))))
  (values))

(defun stop-profiler (&key abort (verbose nil) (profiler *profiler*))
  (let ((p (profiler-process profiler)))
    (setf (profiler-process profiler) nil)
    (when (eq verbose :debug) (format t "~&debug: About to ~a external process" (if abort "SIGKILL" "SIGINT")))
    (ccl:signal-external-process p (if abort #$SIGKILL #$SIGINT) :error-if-exited nil)
    (when (and (ccl:external-process-id p) (eq (ccl:external-process-status p) :running))
      ;; TODO: if abort is true, should time this out in fairly short order.  Maybe don't even
      ;; bother with the semaphore, just sleep a second and then proceed no matter what.
      (ccl:with-interrupts-enabled
        (ccl:wait-on-semaphore (ccl::external-process-completed p) nil "end-profiling")))
    (when (eq verbose :debug) (format t "~&debug: External process completed"))
    (when verbose
      (let ((stream (ccl:external-process-output-stream p)))
        (when (listen stream)
          (fresh-line)
          (loop as ch = (read-char-no-hang stream nil) while ch do (write-char ch))))))
  (setf (profiler-last-result profiler)
        (let ((file (profiler-file profiler)))
          (when (probe-file file)
            (when verbose (format t "~&Parsing ~s" file))
            (read-sample-file file :verbose verbose))))
  (when *impurify-after-profile* 
    (when (eq verbose :debug) (format t "~&debug: Impurifying..."))
    (ccl::impurify)
    (when (eq verbose :debug) (format t "completed")))
  (profiler-last-result profiler))

(defun output-profiling (&key (profiler *profiler*) (file (profiler-output-file profiler)))
  (assert (profiler-last-result profiler) ()
          "No profiling results available in ~s" profiler)
  (setf (profiler-output-file profiler) file)
  (output-sample-html (profiler-last-result profiler) file))

(defun end-profiling (&key abort (verbose nil) output-file (profiler *profiler*))
  (stop-profiler :abort abort :verbose verbose :profiler profiler)
  (unless abort
    (when output-file
      (setf (profiler-output-file profiler) output-file))
    (output-profiling :profiler profiler)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Parsing profiler output file

(defstruct pnode
  count
  description
  children)

(defstruct (named-pnode (:include pnode))
  )

(defstruct (null-pnode (:include named-pnode))
  )

(defstruct (thread-pnode (:include named-pnode))
  id)

(defstruct (kernel-pnode (:include named-pnode))
  name)

(defstruct (address-pnode (:include pnode))
  address
  function
  offset
  ;; For now, when can't find the function, at least record which area it's in
  area)

(defun pnode-address (pnode) (and (address-pnode-p pnode) (address-pnode-address pnode)))
(defun pnode-function (pnode) (and (address-pnode-p pnode) (address-pnode-function pnode)))
(defun pnode-offset (pnode) (and (address-pnode-p pnode) (address-pnode-offset pnode)))

(defmethod print-object ((pnode pnode) stream)
  (print-unreadable-object (pnode stream :type t :identity nil)
    (format stream "~s " (pnode-count pnode))
    (cond ((not (address-pnode-p pnode))
           (format stream "~s" (named-pnode-description pnode)))
          ((address-pnode-function pnode)
           (format stream "~s + ~s" (address-pnode-function pnode) (address-pnode-offset pnode)))
          (t (format stream "0x~x" (address-pnode-address pnode))))
    (format stream " (~s subtree~:p)" (length (pnode-children pnode)))))

(defvar *show-foreign-addresses* nil)

(ccl:defloadvar *kernel-reference-string*
    (format nil "(in ~a)" (pathname-name (car ccl:*command-line-argument-list*))))

(defun parse-sample-line (line)
  (macrolet ((check (form) `(unless ,form (cerror "Weird line ~s" line))))
    (flet ((trimmed-subseq (line start end)
             (loop while (and (< start end) (ccl:whitespacep (char line start))) do (incf start))
             (when (< start end)
               (loop while (ccl:whitespacep (char line (1- end))) do (decf end))
               (subseq line start end))))
      (let* ((end (length line))
             (start (position-if #'alphanumericp line)))
        (check (and start (digit-char-p (char line start))))
        (multiple-value-bind (count npos) (parse-integer line :start start :junk-allowed t)
          (check (< npos end))
          (let* ((lpos (position #\[ line :start npos :from-end t))
                 (desc (or (trimmed-subseq line npos (or lpos end)) "")))
            (if (null lpos)
              (cond ((string= desc "0x0")
                     (make-null-pnode :count count :description desc))
                    ((and (> (length desc) 7) (string= desc "Thread_" :end1 7))
                     (make-thread-pnode :count count :description desc))
                    (t
                     (warn "Missing address in line ~s" line)
                     (make-named-pnode :count count :description desc)))
              (let ((rpos (position #\] line :start lpos)))
                (cond ((string= desc "???  (in <unknown binary>)")
                       (check (null (trimmed-subseq line (1+ rpos) end)))
                       (check (and (< (+ lpos 3) rpos)
                                   (string= line "0x" :start1 (+ lpos 1) :end1 (+ lpos 3))))
                       (make-address-pnode :count count
                                           :description (subseq line (+ lpos 1) rpos)
                                           :address (parse-integer line :start (+ lpos 3) :end rpos :radix 16)))
                      (t
                       (let ((rest (trimmed-subseq line (1+ rpos) end))) ;; source file
                         (when rest
                           (setq desc (concatenate 'string desc " " rest))))
                       (when *show-foreign-addresses*
                         (setq desc (concatenate 'string desc " " (subseq line lpos (1+ rpos)))))
                       (let ((kpos (position #\( desc :from-end t)))
                         (if (and kpos
                                  (string= desc *kernel-reference-string*
                                           :start1 kpos
                                           :end1 (min (+ kpos (length *kernel-reference-string*))
                                                      (length desc))))
                           (make-kernel-pnode :count count
                                               :description desc
                                               :name (trimmed-subseq desc 0 kpos))
                           (make-named-pnode :count count :description desc)))))))))))))

;; Returns list of call trees, one for each thread.
(defun parse-sample-call-graph (stream-or-filename)
  (unless (streamp stream-or-filename)
    (return-from parse-sample-call-graph
      (with-open-file (stream stream-or-filename)
        (parse-sample-call-graph stream))))
  (loop for line = (read-line stream-or-filename) until (equal line "Call graph:"))
  (let ((rstream (cons stream-or-filename nil)))
    (labels ((read-iline (rstream)
               (or (shiftf (cdr rstream) nil)
                   (let* ((line (read-line (car rstream)))
                          (indent (position-if #'digit-char-p line)))
                     (unless (or indent (equal line ""))
                       (cerror "ignore" "Weird line ~s" line)
                       (return-from read-iline (read-iline rstream)))
                     (cons line indent))))
             (unread-iline (rstream iline)
               (assert (null (cdr rstream)) () "Too many unreads in unread-iline")
               (setf (cdr rstream) iline))
             (read-subs (rstream limit)
               (loop
                  for last-indent = nil then indent
                  for iline = (read-iline rstream) as (line . indent) = iline
                  while (and indent (> indent limit))
                  do (assert (or (null last-indent) (eql indent last-indent)) ()
                             "Uneven subtree indent last ~s now ~s" last-indent indent)
                  collect (let ((node (parse-sample-line line)))
                            (setf (pnode-children node) (read-subs rstream indent))
                            (if (eql limit -1)
                              (unless (thread-pnode-p node)
                                (cerror "continue" "Non-thread toplevel entry ~s" line))
                              (when (thread-pnode-p node)
                                (cerror "continue" "Non-toplevel thread entry ~s" line)))
                            node)
                  finally (unread-iline rstream iline))))
      (let ((trees (read-subs rstream -1)))
        (assert (null (cdr (read-iline rstream))))
        trees))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Lookup lisp addresses


#+x86-target
(defun collect-static-functions ()
  (ccl::collect ((functions))
    (ccl::%map-areas (lambda (o)
                       (when (typep o
                               #+x8664-target 'ccl::function-vector
                               #-x8664-target 'function)
                         (functions (ccl::function-vector-to-function o))))
                     :readonly)
    (functions)))


#+(or arm-target ppc-target)
(defun collect-static-functions ()
  (multiple-value-bind (pure-low pure-high)
      (ccl::do-gc-areas (a)
        (when (eql (ccl::%fixnum-ref a target::area.code) ccl::area-readonly)
          (return
            (values (ash (ccl::%fixnum-ref a target::area.low) target::fixnumshift)
                    (ash (ccl::%fixnum-ref a target::area.active) target::fixnumshift)))))
    (let* ((hash (make-hash-table :test #'eq))
           (code-vector-index #+ppc-target 0 #+arm-target 1))
      (ccl::%map-lfuns #'(lambda (f)
                           (let* ((code-vector  (ccl:uvref f code-vector-index))
                                  (startaddr (+ (ccl:%address-of code-vector)
                                                target::misc-data-offset)))
                             (when (and (>= startaddr pure-low)
                                        (< startaddr pure-high))
                               (push f (gethash code-vector hash))))))
      (let* ((n 0))
        (declare (fixnum n))
        (maphash #'(lambda (k v)
                     (declare (ignore k))
                     (if (null (cdr v))
                       (incf n)))
                 hash)
        (let* ((functions ()))
          (maphash #'(lambda (k v)
                       (declare (ignore k))
                       (when (null (cdr v))
                         (push (car v) functions)))
                   hash)
          (sort functions
                #'(lambda (x y)
                    (< (ccl:%address-of (ccl:uvref x code-vector-index))
                       (ccl:%address-of (ccl:uvref y code-vector-index))))))))))




(defun fn-address (fn)
  #+x86-target (ccl:%address-of fn)
  #+ppc-target (- (ccl:%address-of (ccl:uvref fn 0)) (- ppc::fulltag-misc ppc::node-size))
  #+arm-target (- (ccl:%address-of (ccl:uvref fn 1)) (- arm::fulltag-misc arm::node-size)))

(defun fn-size (fn)
  #+x86-target (1+ (ash (1- (ccl::%function-code-words fn)) target::word-shift))
  #+ppc-target (ash (uvsize (ccl:uvref fn 0)) ppc::word-shift)
  #+arm-target (ash (uvsize (ccl:uvref fn 1)) arm::word-shift))

(defun lookup-lisp-addresses (trees)
  (let* ((fns (coerce (collect-static-functions) 'vector))
         (start 0)
         (end (length fns))
         (first (aref fns 0))
         (last (aref fns (1- end)))
         (min (fn-address first))
         (max (+ (fn-address last) (fn-size last) -1)))
    (flet ((fn-for-address (addr)
             (when (<= min addr max)
               (let ((i0 start) (i1 end))
                 (loop
                    (let ((half (ash (+ i0 i1) -1)))
                      (when (eq half i0)
                        (assert (not (eq i0 i1)))
                        (let ((fn (aref fns i0)))
                          (unless (< addr (+ (fn-address fn) (fn-size fn)))
                            (warn "Address #x~x in gap between ~s and ~s"
                                  addr fn (aref fns (1+ i0)))
                            (setq fn nil))
                          (return fn)))
                      (if (< addr (fn-address (aref fns half)))
                        (setq i1 half)
                        (setq i0 half))))))))
      (flet ((update-node (pnode)
               (when (address-pnode-p pnode)
                 (let* ((address (address-pnode-address pnode))
                        (fn (fn-for-address address)))
                   (if fn
                     (setf (address-pnode-function pnode) fn
                           (address-pnode-offset pnode) (- address (fn-address fn)))
                     (setf (address-pnode-area pnode) (find-area-for-address address)))))))
        (labels ((update (tree)
                   (update-node tree)
                   (loop for child in (pnode-children tree) do (update child))))
          (if (consp trees) ;; list of trees
            (loop for tree in trees do (update tree))
            (update trees))
          T)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; clean up the call tree

(defun ignorable-node-p (node)
  (or (null-pnode-p node)
      (and (kernel-pnode-p node)
           (equal "SPFret1valn" (kernel-pnode-name node)))
      (and (address-pnode-p node)
           (member (address-pnode-area node) '(:cstack :tstack :vstack)))))


(defun simplify-sample-tree (tree)
  (labels ((simplify (node)
             ;; Simplify children
             (loop for child in (pnode-children node) do (simplify child))
             ;; Remove ignorable children
             (setf (pnode-children node)
                   (loop for child in (pnode-children node)
                      if (and (ignorable-node-p child)
                              (eql (pnode-count child) ;; no direct count.
                                   (loop for n in (pnode-children child) sum (pnode-count n))))
                      nconc (pnode-children child)
                      else collect child))
             ;; Remove child identical to parent -- I think this happens with recursion, but it's not
             ;; a reliable way to measure recursion anyway, so get rid of it.
             (let ((children (pnode-children node)))
               (when (and children
                          (null (cdr children))
                          (let ((child (car children)))
                            (eq (class-of node) (class-of child))
                            (eq (pnode-count node) (pnode-count child))
                            (equal (pnode-description node) (pnode-description child))))
                 (setf (pnode-children node) (pnode-children (car children))))
               ;; Since child was already simplified, we're done.
               (return-from simplify))))
    (if (consp tree)
        (loop for node in tree do (simplify node))
        (simplify tree))))

(defun read-sample-file (file &key verbose)
  (let ((trees (parse-sample-call-graph file)))
    (when (eq verbose :debug) (format t "~&debug: looking up lisp addresses..."))
    (lookup-lisp-addresses trees)
    (when (eq verbose :debug) (format t "done~%debug: simplifying tree.."))
    ;; Todo: this can take place after impurify...
    (simplify-sample-tree trees)
    (when (eq verbose :debug) (format t "done"))
    trees))



#+debug
(defun find-object-at-address (address)
  (block nil
    (let ((last :before))
      (ccl::%map-areas (lambda (o)
                         (when (< address (ccl:%address-of o))
                           (return last))
                         (setq last o)))
      :after)))

; (find-area-for-address (ccl:%address-of #'(lambda (x) x))) - :dynamic
(defun find-area-for-address (address)
  (macrolet ((ref (a offset) `(ccl::%lisp-word-ref ,a (ash ,offset (- target::fixnumshift)))))
    (loop with shifted-address = (ash address (- target::fixnumshift))
       for area = (ccl::%normalize-areas) then (ref area target::area.succ)
       for code = ccl::area-dynamic then (ref area target::area.code)
       until (eql code ccl::area-void)
       when (<= (ref area target::area.low) shifted-address (ref area target::area.high))
       do (return (ccl::heap-area-name code)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; html output

(defparameter *html-head*  "
  <meta charset=\"utf-8\">
  <script type=\"text/javascript\" src=\"http://ajax.googleapis.com/ajax/libs/jquery/1.11.1/jquery.min.js\"></script>
  <style>
ul {
  list-style: none;
  padding-left: 0.5em;
}
li {
  padding-left: 0.5em;
}
.knob {
  font-family: monospace;
  display: block;
  width: 0.8em;
  height: 0.8em;
  text-align:center;
  line-height: 0.8em;
  border: thin solid transparent;
  float: left;
  margin-right: 0.2em;
}
.phid, .psho {
  border-color: gray;
  cursor:pointer;
}
.pct {
  font-family: monospace;
  font-style: italic;
  background-color: #efffff;
  text-align: right;
}
</style>
<script>
function t_n (this_knob) {
  var knob = $(this_knob);
  knob.toggleClass('phid psho');
  knob.html(knob.hasClass('phid') ? '+' : '-');
  knob.siblings('ul').toggle();
}

$(document).ready(function () {
  $('#wait').hide();
  $('#tree').show();
});
 
</script>
")

(defun output-sample-html (list file)
  (with-open-file (stream file :direction :output :if-exists :supersede)
    (format stream "<!DOCTYPE html>
<html>
<head>
  <title>Profile Call Tree</title>
~A
</head>
<body>
" *html-head*)
    (format stream " <div id='wait'>Loading Call Tree...</div>~%")
    (tree-list-to-html stream list 2 "tree")
    (format stream "
</body>
</html>
")
    (truename stream)))

(defun escape-for-html (string)
  (with-output-to-string (out)
    (with-input-from-string (in string)
      (loop for char = (read-char in nil nil)
            while char
            do (case char
                 ((#\<) (write-string "&lt;" out))
                 ((#\>) (write-string "&gt;" out))
                 ((#\") (write-string "&quot;" out))
                 ((#\') (write-string "&#039;" out))
                 ((#\&) (write-string "&amp;" out))
                 (otherwise (write-char char out)))))))

(defun tree-list-to-html (stream list indent &optional top-id)
  (labels ((p10 (count)
             (declare (fixnum count))
             (loop for p10 fixnum = 10 then (*  10 p10) when (< count p10) return p10))
           (format-tree-line (stream node max10)
	     (let* ((fn (pnode-function node))
                    (count (pnode-count node))
		    (namestr (if fn
                               (escape-for-html
                                (format nil "~s + ~d" fn (pnode-offset node)))
                               (if (address-pnode-p node)
                                 (format nil "0x~x (~A)" (address-pnode-address node)
                                         (or (address-pnode-area node) :unknown))
                                 (escape-for-html 
                                  (pnode-description node))))))
               (write-string "<span class='pct'>" stream)
               (loop while (< count (setq max10 (/ max10 10))) do (write-string "&nbsp;" stream))
               (format stream "~d</span> ~a" count namestr)))
	   (output-lis (stream list indent top-id &optional max10)
	     (when list
               (format stream "~&~vt<ul~@[ id=\"~a\"~] style=\"display:none;\">~%" indent top-id)
               (unless max10
                 (setq max10 (p10 (loop for node in list maximize (pnode-count node)))))
	       (let* ((indent (+ indent 1)))
		 (loop for tree in list do (output-li stream tree indent max10)))
	       (format stream "~&~vt</ul>" indent)))
	   (output-li (stream node indent max10)
	     (format stream "~&~vt<li>" indent)
             ;; Setting this up with javascript is too slow on large trees, so just put it inline.
             (if (pnode-children node)
               (format stream "<span class='knob phid' onclick='t_n(this);'>+</span>")
               (format stream "<span class='knob'>&nbsp;</span>"))
	     (format-tree-line stream node max10)
	     (output-lis stream (pnode-children node) (1+ indent) nil max10)
	     (format stream "</li>~%")))
    (output-lis stream list indent top-id)))

