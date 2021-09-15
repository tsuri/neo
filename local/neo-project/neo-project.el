;;; neo-project.el --- A project management infrastructure

(require 'cl-lib)

(define-derived-mode neo/project-mode tabulated-list-mode "Project"
  "Neo Project  mode"
  (let ((columns [("Jira" 8) ("Status" 12) ("Desc" 40)])
        (rows (list '(nil ["AV-15412" "code review" "This is a nice feature to have"])
                  '(nil ["AV-15412" "queued" "This is also not too bad"])
                  '(nil ["AV-15415" "merged" "This is probably a waste of time"]))))
    (setq tabulated-list-format columns)
    (setq tabulated-list-entries rows)
    (tabulated-list-init-header)
    (tabulated-list-print)))

(defun neo/project-generate-new-buffer (mode)
  (let ((buffer (generate-new-buffer "neo-project")))
    (with-current-buffer buffer
      (funcall mode))
    buffer))

(defun neo/project-setup-buffer ()
  ""
  (let ((buffer (neo/project-generate-new-buffer #'neo/project-mode)))))


(defun neo/project ()
  (interactive)
  (switch-to-buffer "*project*")
  (neo/project-mode))

(provide 'neo-project)

(cl-eval-when (load eval)
  (require 'neo-project-db))

;;; neo-project.el ends here

(use-package hierarchy)
(require 'hierarchy)

(defun hierarchy-examples-faces--parent (face)
  "Return parent face of FACE.
If FACE doesn't inherit from any other face or inherit from an
invalid face, return 'root-face."
  (let ((parent-face (if (eq face 'root-face)
                         nil ;; the root has no parent
                       (or (face-attribute face :inherit nil 'default)
                           'root-face))))
    (cond ((facep parent-face) parent-face)
          ((null parent-face) nil) ;; the root has no parent
          (t 'root-face))))

(defun hierarchy-examples-faces--build-hierarchy ()
  "Return a hierarchy of all faces Emacs knows about."
  (let ((hierarchy (hierarchy-new)))
    (hierarchy-add-trees hierarchy (face-list) #'hierarchy-examples-faces--parent)
    hierarchy))

(defun hierarchy-examples-faces-display-faces ()
  "Display hierarchy of all faces Emacs knows about in a tree widget."
  (interactive)
  (require 'tree-widget)
  (let* ((hierarchy (hierarchy-examples-faces--build-hierarchy)))
    (switch-to-buffer
     (hierarchy-tree-display
      hierarchy
      (lambda (face _) (insert (format "%s" face)))))))


(require 'hierarchy)

(defun hierarchy-examples-fs-directory-p (file)
  "Return non-nil if FILE is a directory and not . or ..."
  (and (not (string-suffix-p "/." file))
       (not (string-suffix-p "/.." file))
       (file-directory-p file)))

(defun hierarchy-examples-fs-children (folder)
  "Return sub-directories of FOLDER as absolute paths."
  (when (file-directory-p folder)
    (seq-filter #'hierarchy-examples-fs-directory-p (directory-files folder t))))

(defun hierarchy-examples-fs-parent (folder)
  "Return parent of FOLDER."
  (when (not (string= folder "/"))
    (directory-file-name (file-name-directory folder))))

(defun hierarchy-examples-fs-build-fs-hierarchy (folder)
  "Return hierarchy of FOLDER."
  (let* ((folder (expand-file-name folder))
         (parentfn #'hierarchy-examples-fs-parent)
         (childrenfn (lambda (file) (when (string-prefix-p folder file)
                                 (hierarchy-examples-fs-children file))))
         (hierarchy (hierarchy-new)))
    (hierarchy-add-tree hierarchy folder parentfn childrenfn)
    (hierarchy-sort hierarchy)
    hierarchy))

(defun hierarchy-examples-fs-labelfn (folder _)
  "Insert name of FOLDER at current position.

_ is ignored."
  (insert (if (string= folder "/")
              "/"
            (file-name-nondirectory folder))))

(defun hierarchy-examples-fs-display-filesystem (&optional folder)
  "Display hierarchy of FOLDER in a tabulated list."
  (let* ((hierarchy (hierarchy-examples-fs-build-fs-hierarchy folder))
         (buffer (hierarchy-tabulated-display
                  hierarchy
                  (hierarchy-labelfn-indent
                   (hierarchy-labelfn-button
                    #'hierarchy-examples-fs-labelfn (lambda (item _) (dired item)))))))
    (switch-to-buffer buffer)))

(defun hierarchy-examples-fs-display-filesystem-tree (&optional folder)
  "Display hierarchy of FOLDER in a tree widget."
  (require 'tree-widget)
  (let* ((hierarchy (hierarchy-examples-fs-build-fs-hierarchy folder)))
    (switch-to-buffer (hierarchy-tree-display hierarchy #'hierarchy-examples-fs-labelfn))))

;; Execute one of the following lines to show the .emacd.d hierarchy
;; in either a tabulated list or a tree widget. This takes around 4
;; seconds on my computer.
;;
;; (hierarchy-examples-fs-display-filesystem "~/.emacs.d")
;;
;; (hierarchy-examples-fs-display-filesystem-tree "~/.emacs.d")
