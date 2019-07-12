;;; epdh.el --- Code useful for developing Emacs packages -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
;; Keywords: convenience, development
;; URL: https://github.com/alphapapa/emacs-package-dev-handbook
;; Package-Requires: ((emacs "25.1") (dash "2.13") (s "1.10.0"))

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Code useful for developing Emacs packages.  Contributions welcome.

;;; Code:

;;;; Requirements

(require 'cl-lib)

(require 'dash)
(require 's)

;;;; General tools

;;;###autoload
(defun epdh/byte-compile-and-load-directory (directory)
  "Byte-compile and load all elisp files in DIRECTORY.
Interactively, directory defaults to `default-directory' and asks
for confirmation."
  (interactive (list default-directory))
  (if (or (not (called-interactively-p))
          (yes-or-no-p (format "Compile and load all files in %s?" directory)))
      ;; Not sure if binding `load-path' is necessary.
      (let* ((load-path (cons directory load-path))
             (files (directory-files directory 't (rx ".el" eos))))
        (dolist (file files)
          (byte-compile-file file 'load)))))

;;;###autoload
(defun epdh/emacs-lisp-macroreplace ()
  "Replace macro form before or after point with its expansion."
  (interactive)
  (if-let* ((beg (point))
            (end t)
            (form (or (ignore-errors
                        (save-excursion
                          (prog1 (read (current-buffer))
                            (setq end (point)))))
                      (ignore-errors
                        (forward-sexp -1)
                        (setq beg (point))
                        (prog1 (read (current-buffer))
                          (setq end (point))))))
            (expansion (macroexpand-all form)))
      (setf (buffer-substring beg end) (pp-to-string expansion))
    (user-error "Unable to expand")))

;;;; Profiling / Optimization

;;;###autoload
(cl-defmacro bench (&optional (times 100000) &rest body)
  "Call `benchmark-run-compiled' on BODY with TIMES iterations, returning list suitable for Org source block evaluation.
Garbage is collected before calling `benchmark-run-compiled' to
avoid counting existing garbage which needs collection."
  (declare (indent defun))
  `(progn
     (garbage-collect)
     (list '("Total runtime" "# of GCs" "Total GC runtime")
           'hline
           (benchmark-run-compiled ,times
             (progn
               ,@body)))))

;;;###autoload
(cl-defmacro bench-multi (&key (times 1) forms ensure-equal raw)
  "Return Org table as a list with benchmark results for FORMS.
Runs FORMS with `benchmark-run-compiled' for TIMES iterations.

When ENSURE-EQUAL is non-nil, the results of FORMS are compared,
and an error is raised if they aren't `equal'. If the results are
sequences, the difference between them is shown with
`seq-difference'.

When RAW is non-nil, the raw results from
`benchmark-run-compiled' are returned instead of an Org table
list.

If the first element of a form is a string, it's used as the
form's description in the bench-multi-results; otherwise, forms
are numbered from 0.

Before each form is run, `garbage-collect' is called."
  ;; MAYBE: Since `bench-multi-lexical' byte-compiles the file, I'm not sure if
  ;; `benchmark-run-compiled' is necessary over `benchmark-run', or if it matters.
  (declare (indent defun))
  (let*((keys (gensym "keys"))
        (result-times (gensym "result-times"))
        (header '(("Form" "x faster than next" "Total runtime" "# of GCs" "Total GC runtime")
                  hline))
        ;; Copy forms so that a subsequent call of the macro will get the original forms.
        (forms (copy-list forms))
        (descriptions (cl-loop for form in forms
                               for i from 0
                               collect (if (stringp (car form))
                                           (prog1 (car form)
                                             (setf (nth i forms) (cadr (nth i forms))))
                                         i))))
    `(unwind-protect
         (progn
           (defvar bench-multi-results nil)
           (let* ((bench-multi-results (make-hash-table))
                  (,result-times (sort (list ,@(cl-loop for form in forms
                                                        for i from 0
                                                        for description = (nth i descriptions)
                                                        collect `(progn
                                                                   (garbage-collect)
                                                                   (cons ,description
                                                                         (benchmark-run-compiled ,times
                                                                           ,(if ensure-equal
                                                                                `(puthash ,description ,form bench-multi-results)
                                                                              form))))))
                                       (lambda (a b)
                                         (< (second a) (second b))))))
             ,(when ensure-equal
                `(cl-loop with ,keys = (hash-table-keys bench-multi-results)
                          for i from 0 to (- (length ,keys) 2)
                          unless (equal (gethash (nth i ,keys) bench-multi-results)
                                        (gethash (nth (1+ i) ,keys) bench-multi-results))
                          do (if (sequencep (gethash (car (hash-table-keys bench-multi-results)) bench-multi-results))
                                 (let* ((k1) (k2)
                                        ;; If the difference in one order is nil, try in other order.
                                        (difference (or (setq k1 (nth i ,keys)
                                                              k2 (nth (1+ i) ,keys)
                                                              difference (seq-difference (gethash k1 bench-multi-results)
                                                                                         (gethash k2 bench-multi-results)))
                                                        (setq k1 (nth (1+ i) ,keys)
                                                              k2 (nth i ,keys)
                                                              difference (seq-difference (gethash k1 bench-multi-results)
                                                                                         (gethash k2 bench-multi-results))))))
                                   (user-error "Forms' bench-multi-results not equal: difference (%s - %s): %S"
                                               k1 k2 difference))
                               ;; Not a sequence
                               (user-error "Forms' bench-multi-results not equal: %s:%S %s:%S"
                                           (nth i ,keys) (nth (1+ i) ,keys)
                                           (gethash (nth i ,keys) bench-multi-results)
                                           (gethash (nth (1+ i) ,keys) bench-multi-results)))))
             ;; Add factors to times and return table
             (if ,raw
                 ,result-times
               (append ',header
                       (bench-multi-process-results ,result-times)))))
       (unintern 'bench-multi-results nil))))

(defun bench-multi-process-results (results)
  "Return sorted RESULTS with factors added."
  (setq results (sort results (-on #'< #'second)))
  (cl-loop with length = (length results)
           for i from 0 to (1- length)
           for description = (car (nth i results))
           for factor = (if (< i (1- length))
                            (format "%.2f" (/ (second (nth (1+ i) results))
                                              (second (nth i results))))
                          "slowest")
           collect (append (list description factor)
                           (list (format "%.6f" (second (nth i results)))
                                 (third (nth i results))
                                 (if (> (fourth (nth i results)) 0)
                                     (format "%.6f" (fourth (nth i results)))
                                   0)))))

;;;###autoload
(cl-defmacro bench-multi-lexical (&key (times 1) forms ensure-equal raw)
  "Return Org table as a list with benchmark results for FORMS.
Runs FORMS from a byte-compiled temp file with `lexical-binding'
enabled, using `bench-multi', which see.

Afterward, the temp file is deleted and the function used to run
the benchmark is uninterned."
  (declare (indent defun))
  `(let* ((temp-file (concat (make-temp-file "bench-multi-lexical-") ".el"))
          (fn (gensym "bench-multi-lexical-run-")))
     (with-temp-file temp-file
       (insert ";; -*- lexical-binding: t; -*-" "\n\n"
               "(defvar bench-multi-results)" "\n\n"
               (format "(defun %s () (bench-multi :times %d :ensure-equal %s :raw %s :forms %S))"
                       fn ,times ,ensure-equal ,raw ',forms)))
     (unwind-protect
         (if (byte-compile-file temp-file 'load)
             (funcall (intern (symbol-name fn)))
           (user-error "Error byte-compiling and loading temp file"))
       (delete-file temp-file)
       (unintern (symbol-name fn) nil))))

;;;###autoload
(cl-defmacro bench-dynamic-vs-lexical-binding (&key (times 1) forms ensure-equal)
  "Benchmark FORMS with both dynamic and lexical binding.
Calls `bench-multi' and `bench-multi-lexical', which see."
  (declare (indent defun))
  `(let ((dynamic (bench-multi :times ,times :ensure-equal ,ensure-equal :raw t
                    :forms ,forms))
         (lexical (bench-multi-lexical :times ,times :ensure-equal ,ensure-equal :raw t
                    :forms ,forms))
         (header '("Form" "x faster than next" "Total runtime" "# of GCs" "Total GC runtime")))
     (cl-loop for result in-ref dynamic
              do (setf (car result) (format "Dynamic: %s" (car result))))
     (cl-loop for result in-ref lexical
              do (setf (car result) (format "Lexical: %s" (car result))))
     (append (list header)
             (list 'hline)
             (bench-multi-process-results (append dynamic lexical)))))

;;;###autoload
(defmacro elp-profile (times prefixes &rest body)
  (declare (indent defun))
  `(let (output)
     (dolist (prefix ,prefixes)
       (elp-instrument-package (symbol-name prefix)))
     (dotimes (x ,times)
       ,@body)
     (elp-results)
     (elp-restore-all)
     (point-min)
     (forward-line 20)
     (delete-region (point) (point-max))
     (setq output (buffer-substring-no-properties (point-min) (point-max)))
     (kill-buffer)
     (delete-window)
     (let ((rows (s-lines output)))
       (append (list (list "Function" "Times called" "Total time" "Average time")
                     'hline)
               (cl-loop for row in rows
                        collect (s-split (rx (1+ space)) row 'omit-nulls))))))

;;;; Footer

(provide 'epdh)

;;; epdh.el ends here
