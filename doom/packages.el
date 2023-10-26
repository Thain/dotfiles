;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with DOOM:
;; - declare them here
;; - run 'doom sync'
;; - restart, or use doom/reload (spc h r r)


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)
(package! web-mode)
(package! org-roam)
(package! org-journal)
;;(package! fish-mode)
(package! vterm)
;;(package! mastodon)
(package! org-modern)
(package! svg-tag-mode)
(package! org-superstar)
(package! olivetti)
(package! ranger)
(package! org-caldav)

;; org appear to show rich text contents at point
(package! org-appear
  :recipe (:host github
           :repo "awth13/org-appear"))

(package! lambda-line
  :recipe (:host github
           :repo "Lambda-Emacs/lambda-line"))

;; mode for writing kmonad config
(package! kbd-mode
  :recipe (:host github
           :repo "kmonad/kbd-mode"))

(package! aas
  :recipe (:host github
           :repo "ymarco/auto-activating-snippets"))


;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; To disable a package included with Doom, use the `:disable' property:
;(package! builtin-package :disable t)
;;(package! company :disable t)
;;(package! modeline :disable t)
;;(package! snippets :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)
