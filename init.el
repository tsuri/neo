;;; init.el -*- lexical-binding: t; -*-

(require 'subr-x) ; for string-remove-suffix

;;; We will be using no-littering, but we need first to tell straight to keep
;;; repo and build directories out of the way
(defun neo/litter-directory (&optional leaf-dir version)
  (let* ((dir (directory-file-name
               (file-name-directory user-emacs-directory)))
         (distribution (string-remove-suffix ".d" dir))
	 (package-dir (if leaf-dir leaf-dir ""))
         (version-dir (if version version "")))
    (file-name-as-directory (format "%s-save.d/%s/%s" distribution package-dir version-dir))))

(setq straight-base-dir (neo/litter-directory))
(setq straight-build-dir (neo/litter-directory "straight/build" emacs-version))

;;; streaight.el bootstrap code from https://github.com/raxod502/straight.el#getting-started
;;; NOTE: the directory used to be user-emacs-directory, we are moving packages
;;; out of our configuration and in places compatible with no-littering
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" straight-base-dir))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;; Install org from upstream repo before using org-babel
(straight-use-package '(org
			:type git
			:repo "https://code.orgmode.org/bzg/org-mode.git"
			:local-repo "org"
			:depth full
			:pre-build (straight-recipes-org-elpa--build)
			:build (:not autoloads)
			:files (:defaults "lisp/*.el" ("etc/styles/" "etc/styles/*"))))

(straight-use-package 'use-package)
;; Configure use-package to use straight.el by default
(use-package straight
  :custom (straight-use-package-by-default t))

(org-babel-load-file (concat (file-name-as-directory user-emacs-directory) "neo.org"))
