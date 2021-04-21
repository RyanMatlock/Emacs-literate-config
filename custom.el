(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#839496" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#002b36"))
 '(chess-display-highlight-legal t)
 '(compilation-message-face 'default)
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   '("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" default))
 '(ebib-bibtex-dialect 'biblatex)
 '(emojify-display-style 'image)
 '(eshell-cmpl-cycle-completions nil)
 '(fci-rule-color "#073642")
 '(flycheck-python-flake8-executable "python3")
 '(flycheck-python-pycompile-executable "python3")
 '(flycheck-python-pylint-executable "python3")
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-type 'auto)
 '(haskell-tags-on-save t)
 '(highlight-changes-colors '("#d33682" "#6c71c4"))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#002b36" 0.25)
    '("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2")))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   '(("#073642" . 0)
     ("#546E00" . 20)
     ("#00736F" . 30)
     ("#00629D" . 50)
     ("#7B6000" . 60)
     ("#8B2C02" . 70)
     ("#93115C" . 85)
     ("#073642" . 100)))
 '(hl-bg-colors
   '("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00"))
 '(hl-fg-colors
   '("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36"))
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(ido-file-extensions-order '(".org" ".py" ".tex" ".el" ".txt" ".yaml" ".js" ".md"))
 '(initial-major-mode 'text-mode)
 '(initial-scratch-message nil)
 '(magit-diff-use-overlays nil)
 '(org-adapt-indentation nil)
 '(org-export-backends '(ascii beamer html icalendar latex md odt))
 '(org-export-with-smart-quotes t)
 '(org-hide-emphasis-markers t)
 '(org-hide-leading-stars nil)
 '(org-highlight-latex-and-related '(native))
 '(org-journal-date-format "%A, %F")
 '(org-journal-dir "~/Dropbox/journal/")
 '(org-journal-file-format "%Y/%m/week-%V-(%Y-%m-%d).org")
 '(org-journal-file-type 'weekly)
 '(org-journal-time-format "[%I:%M%P]")
 '(org-latex-default-packages-alist
   '(("AUTO" "inputenc" t
      ("pdflatex"))
     ("T1" "fontenc" t
      ("pdflatex"))
     ("" "graphicx" t nil)
     ("" "grffile" t nil)
     ("" "longtable" nil nil)
     ("" "wrapfig" nil nil)
     ("" "rotating" nil nil)
     ("normalem" "ulem" t nil)
     ("" "amsmath" t nil)
     ("" "textcomp" t nil)
     ("" "amssymb" t nil)
     ("" "capt-of" nil nil)
     ("colorlinks=true" "hyperref" nil nil)
     ("" "siunitx" nil nil)))
 '(org-mode-hook
   '(;; hookify:my:org-list-note-prompt
     ;; org-extra-yas-mode-activation-kludge
     ;; hookify:my:org-html-image-width
     ob-ipython-auto-configure-kernels
     ;; hookify:my:org-insert-visible-space-char
     ;; hookify:my:org-insert-checkbox-item
     ;; my:org-insert-bullet
     ;; my:insert-corner-brackets
     ;; my:insert-right-corner-bracket
     ;; my:insert-left-corner-bracket
     ;; my:insert-micro-sign
     ;; my:insert-section-sign
     ;; my:org-priority-key
     ;; my:org-time-stamp-key
     flyspell-mode
     org-tempo-setup
     #[0 "\300\301\302\303\304$\207"
         [add-hook change-major-mode-hook org-show-all append local]
         5]
     #[0 "\300\301\302\303\304$\207"
         [add-hook change-major-mode-hook org-babel-show-result-all append local]
         5]
     org-babel-result-hide-spec
     org-babel-hide-all-hashes
     #[0 "\301\211\207"
         [imenu-create-index-function org-imenu-get-tree]
         2]
     (lambda nil
       (hs-minor-mode))
     org-journal-update-auto-mode-alist))
 '(org-pretty-entities t)
 '(package-archives
   '(("melpa" . "https://melpa.org/packages/")
     ("gnu" . "https://elpa.gnu.org/packages/")))
 '(package-selected-packages
   '(hindent lsp-jedi lsp-latex lsp-pyright lsp-haskell flymake-hlint haskell-tab-indent mediawiki vterm osx-clipboard company-org-roam org-roam org-roam-bibtex org-roam-server org-scrum chess company-lsp osx-dictionary lsp-ui flycheck-rust lsp-mode rust-mode toml-mode elpy pomidor unfill sql-indent sqlup-mode db-pg latex-math-preview org-ref ox-reveal highlight-indent-guides org-journal org-journal-list ox-pandoc ox-rst ox-tufte ox-wk org-re-reveal lorem-ipsum company-anaconda company-bibtex company-emoji company-math company-shell company-web telephone-line magit magit-filenotify transient solarized-theme theme-looper flx flx-ido ido-yes-or-no ivy-yasnippet emoji-display ox-epub ivy ebib hideshow-org s ob-ipython ein electric-case electric-operator electric-spacing elein eldoc-overlay eldoc-eval ac-slime elisp-slime-nav slime auto-complete emojify yaml-mode wgrep-ack wget web-beautify tagedit sx scion pytest paredit pandoc-mode org-bullets org-ac nodejs-repl json-mode js3-mode iedit help-mode+ help-fns+ help+ helm-bibtex gist flycheck-pyflakes exec-path-from-shell d-mode company-auctex cider blank-mode bison-mode better-defaults awk-it auto-complete-chunk auto-complete-c-headers auctex-latexmk arduino-mode ac-python ac-js2 ac-geiser))
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(show-trailing-whitespace t)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(solarized-distinct-fringe-background t)
 '(solarized-high-contrast-mode-line t)
 '(solarized-scale-org-headlines nil)
 '(solarized-use-more-italic t)
 '(solarized-use-variable-pitch nil)
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(text-mode-hook
   '(turn-on-flyspell
     ;; my:org-insert-bullet
     ;; my:insert-corner-brackets
     ;; my:insert-right-corner-bracket
     ;; my:insert-left-corner-bracket
     turn-on-auto-fill
     text-mode-hook-identify))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   '((20 . "#dc322f")
     (40 . "#cb4b16")
     (60 . "#b58900")
     (80 . "#859900")
     (100 . "#2aa198")
     (120 . "#268bd2")
     (140 . "#d33682")
     (160 . "#6c71c4")
     (180 . "#dc322f")
     (200 . "#cb4b16")
     (220 . "#b58900")
     (240 . "#859900")
     (260 . "#2aa198")
     (280 . "#268bd2")
     (300 . "#d33682")
     (320 . "#6c71c4")
     (340 . "#dc322f")
     (360 . "#cb4b16")))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   '(unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83"))
 '(whitespace-style '(face lines-tail)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-level-1 ((t (:inherit default :foreground "#cb4b16" :weight bold))))
 '(org-level-2 ((t (:inherit default :foreground "#859900" :weight semi-bold))))
 '(org-level-3 ((t (:inherit default :foreground "#268bd2" :weight semi-bold)))))
