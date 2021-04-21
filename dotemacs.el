;; Usage
;; symlink ~/.emacs to this file in order to execute Emacs Lisp code in
;; literate Org mode Emacs config file, i.e.
;; $ ln -s /path/to/dotemacs.el ~/.emacs

;; see https://cestlaz.github.io/posts/using-emacs-10-org-init/
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(org-babel-load-file (expand-file-name "~/config/emacs/README.org"))
