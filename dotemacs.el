;; Usage
;; symlink ~/.emacs to this file in order to execute Emacs Lisp code in
;; literate Org mode Emacs config file, i.e.
;; $ ln -s /path/to/dotemacs.el ~/.emacs

(setq comp-deferred-compilation t)

;; Bootstrap straight.el (package.el replacement)
;; see https://github.com/raxod502/straight.el
;; and https://youtu.be/UmbVeqphGlc
;; (straight.el: Advanced Emacs Package Management | YouTube)
;; viewed 2022-01-16
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
                         user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         (concat "https://raw.githubusercontent.com/raxod502/"
                 "straight.el/develop/install.el")
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; ;; see https://cestlaz.github.io/posts/using-emacs-10-org-init/
;; (require 'package)
;; (setq package-enable-at-startup nil)
;; (add-to-list 'package-archives
;;              '("melpa" . "https://melpa.org/packages/"))
;; (package-initialize)

;; ;; Bootstrap `use-package'
;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))

;; stolen from https://github.com/petercheng00/emacs/blob/master/init.el
;; which has a nice, literate Org mode Emacs config
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(use-package org)

(org-babel-load-file (expand-file-name "~/config/emacs/core-config.org"))
