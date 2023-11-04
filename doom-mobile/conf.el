(setq user-full-name "Liam Chung"
      user-mail-address "lwalkerchung@gmail.com")

(defun doom-dash-ascii ()
  (let* ((banner
          '(",---.,-.-.,---.,---.,---."
            "|---'| | |,---||    `---."
            "`---'` ' '`---^`---'`---'"))
         (longest-line (apply #'max (mapcar #'length banner))))
    (put-text-property
     (point)
     (dolist (line banner (point))
       (insert (+doom-dashboard--center
                +doom-dashboard--width
                (concat line (make-string (max 0 (- longest-line (length line))) 32)))
               "\n"))
     'face 'doom-dashboard-banner)))

(setq +doom-dashboard-ascii-banner-fn #'doom-dash-ascii)
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-footer)
(setq-hook! '+doom-dashboard-mode-hook cursor-type nil)
(setq-hook! '+doom-dashboard-mode-hook evil-normal-state-cursor nil)

;;alternatives: doom-dracula, doom-one, doom-solarized-dark, doom-palenight
;(setq doom-theme 'doom-nord)
(setq doom-theme 'doom-one)
;(setq doom-theme 'doom-nano-dark)
;(setq doom-theme 'doom-palenight)
(setq line-move-visual nil
      display-line-numbers-type nil
      doom-font (font-spec :family "FiraCode Nerd Font" :size 18 :weight 'regular)
      confirm-kill-emacs nil)

(map! :leader
      (:prefix ("TAB" . "workspace")
        :desc "load workspace"              "SPC" #'+workspace/load
        :desc "workspace forward"             "l" #'+workspace/switch-right
        :desc "workspace back"                "h" #'+workspace/switch-left))

(map! :leader
      (:prefix ("b" . "buffer")
        :desc "Kill buffer and window"        "k" #'kill-buffer-and-window))

(map! :leader
      (:prefix ("d" . "dired")
        :desc "Open dired in specified dir"   "." #'dired
        :desc "Open dired in $HOME"           "h" (lambda () (interactive) (dired "~"))
        :desc "Open dired in root"            "r" (lambda () (interactive) (dired "/"))
        :desc "Open dired in org dir"         "o" (lambda () (interactive) (dired org-directory))
        :desc "Close all dired buffers"       "q" #'+dired/quit-all
        :desc "Open dired in PWD"             "d" #'dired-jump))

(map! :leader
      (:prefix ("o" . "open")
        :desc "Open agenda"                   "a" #'org-agenda
        :desc "Open terminal"                 "s" #'vterm
        :desc "Open media player"             "e" #'emms
        :desc "Open calendar"                 "c" #'=calendar
        :desc "Open notes (notes.org)"        "n" (lambda () (interactive) (find-file (concat org-directory "/notes.org" )))
        :desc "Open inbox"                    "i" (lambda () (interactive) (find-file (concat org-directory "/gtd/inbox.org" )))
        :desc "Open config (conf.org)"        "C" (lambda () (interactive) (find-file "~/.config/doom/conf.org"))
        :desc "Open $HOME in dired"           "h" (lambda () (interactive) (dired "~"))
        :desc "Open root in dired"            "r" (lambda () (interactive) (dired "/"))
        :desc "Open todo"                     "t" #'org-todo-list))

(map! :leader
      (:prefix ("w" . "window")
        :desc "open file in other window"    "." #'find-file-other-window
        :desc "vsplit and follow"            "v" #'+evil/window-vsplit-and-follow
        :desc "split and follow"             "s" #'+evil/window-split-and-follow))

(map! :leader
    ;; notes. not really necessary for what little i do,
    ;; so i've bound the ones i need over to the "open" and "dired" prefixes
    "n" nil
    ;; git, get to it when i need it
    "g" nil
    ;; actions; a wild list of things. "embark" is a recurring word
    "a" nil
    ;; resume last search
    "'" nil
    ;; toggle last popup
    "~" nil
    ;; universal arg?
    "u" nil
    ;; search. should get a dict/thesaur backend
    "s" nil
    ;; code. need modules for it?
    "c" nil
    ;; toggle. good for toggling modes and variables, but i dont have
    ;; specific things in mind yet. would like one for visual line mode
    "t" nil
    (:prefix ("o" . "open")
        "b" nil "-" nil "E" nil
        "l" nil "L" nil "u" nil "R" nil
        "U" nil "A" nil "d" nil)
    (:prefix ("h" . "help")
        "l" #'+lookup/definition
        ;; apropos. seems like a documentation thing
        ;"a" nil
        "A" nil
        ;; describe coding system. no idea
        "C" nil
        ;; describe various things I don't need
        ;; face, gnu,   input,   news,   syntax, manorwoman, lang
        "g" nil "l" nil "n" nil "s" nil "W" nil     "L" nil
        ;; autodef, profiler, info other window
        "u" nil     "T" nil   "4" nil "h" nil
        ;; alternate versions of things i already got rid of
        ;; and will certainly never use these keybinds for em
        "C-a" nil "C-c" nil "C-d" nil "C-e" nil "C-f" nil "C-k" nil
        "C-l" nil "C-n" nil "C-o" nil "C-p" nil "C-s" nil "C-t" nil
        "C-w" nil "<f1>" nil "q" nil  "C-\\" nil
        ;; various things maybe I will eventually want, but not yet
        ;; command, goto docs key, sandbox, local help, char
        "x" nil     "K" nil        "E" nil  "." nil     "'" nil
        ;; info, symbol, help packages, library
        "i" nil "o" nil "p" nil "P" nil
        ;; input history, online lookup, load theme, whereis
        "I" nil          "O" nil        "t" nil     "w" nil)
    (:prefix ("b" . "buffer")
        "n" nil "z" nil "Z" nil
        "x" nil "u" nil "p" nil
        "N" nil "r" nil "O" nil)
    (:prefix ("f" . "file")
        "c" nil "e" nil "E" nil "l" nil
        "P" nil "F" nil "u" nil "U" nil)
    (:prefix ("w" . "window")
        "C-S-h" nil "C-S-j" nil "C-S-k" nil "C-S-l" nil "C-S-r" nil "C-S-s" nil "C-S-w" nil
        "C-b" nil "C-c" nil "C-f" nil "C-h" nil "C-j" nil "C-l" nil "C-w" nil "C-v" nil
        "C-n" nil "C-o" nil "C-p" nil "C-r" nil "C-s" nil "C-t" nil "C-u" nil "C-k" nil
        "C-q" nil "C-x" nil "C-_" nil "<left>" nil "<right>" nil
        "C-<left>" nil "C-<right>" nil "<up>" nil "C-<down>" nil "<down>" nil "C-<up>" nil
        "c" nil "t" nil "b" nil "n" nil "m" nil
        ;; I don't really separate windows. should i?
        "T" nil
        ;; TODO a bunch of evil binds. what are these supposed to do
        "g" nil "f" nil
        ;; don't need these since i made split and follow the default
        "S" nil "V" nil
        ;; TODO what are these supposed to do
        "r" nil "R" nil "p" nil
        ;; TODO what is the difference between these two and spc w d
        "q" nil "c" nil
        ;; TODO why do these do weird shit "_" nil
        "|" nil))

(map! :leader :desc "Open org capture"   "c" #'org-capture )

(evil-select-search-module 'evil-search-module 'isearch)

;;(after! evil (setq evil-want-minibuffer 1))
;; TODO bind an ex search and replace
(map! :map evil-motion-state-map :prefix "g"
      :desc "goto line number"                 "o" 'evil-goto-line
      :desc "cursor to end of line"            "l" 'evil-end-of-line
      :desc "nonblank start of line"           "h" 'evil-first-non-blank
      "$" nil "^" nil "e" nil "E" nil ;; removing binds
      "K" nil "n" nil "N" nil "t" nil "T" nil "u" nil "U" nil "C-]" nil
      "C-g" nil "<down>" nil "<up>" nil "<home>" nil "<end>" nil "#" nil "*" nil
      "0" nil "_" nil
      ;; a lot of stuff in here, esp related to avy it seems. worth exploring eventually
      "s" nil
      )

(map! :map evil-visual-state-map :prefix "g"
      :desc "cursor to end of line"            "l" 'evil-end-of-line
      :desc "nonblank start of line"           "h" 'evil-first-non-blank
      )

(map! :map evil-normal-state-map :prefix "g"
      :desc "invert case of selection"         "`" 'evil-invert-case
      "8" nil "&" nil "@" nil "?" nil "~" nil "," nil "a" nil "A" nil "f" nil "F" nil
      "I" nil "J" nil "l" nil "L" nil "P" nil "y" nil
      ;; maybe bring these back when i'm ready. lots of lookup for files and references
      "d" nil "D" nil "f" nil "O" nil
      ;; what do these do? evil-fill
      "w" nil "q" nil
      ;; how does eval work? both for selection and whole buffer
      "r" nil "R" nil
      ;; some workspace stuff?
      "t" nil "T" nil
      ;; a lot of stuff in here, esp related to avy it seems. worth exploring eventually
      "s" nil
      )

(after! org
  (setq org-directory "~/storage/shared/Documents/org"
        ;; todo vars
        org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "WAIT(w)" "DONE(d)" ))
        ;;org-todo-keyword-faces '(("TODO" . "magenta"))
        org-use-fast-todo-selection 'expert
        ;; agenda vars
        org-agenda-skip-scheduled-if-done t
        org-agenda-files (list (concat org-directory "/gtd/" ))
        ;; startup vars
        org-startup-folded t
        org-startup-indented nil
        ;; org-startup-with-inline-images t
        ;; fontifying
        org-fontify-done-headline t
        org-fontify-whole-heading-line t
        org-fontify-quote-and-verse-blocks t
        org-src-fontify-natively t
        ;; other
        org-cycle-include-plain-lists 'integrate ; plain lists start folded
        org-tags-column -60
        org-ellipsis " [±] " ;; folding symbol
        org-agenda-block-separator ""
        org-list-allow-alphabetical t   ; have a. A. a) A) list bullets
        org-fold-catch-invisible-edits 'smart ; don't brazenly edit things you can't see

        org-refile-targets '(("gtd/admin.org" :maxlevel . 2)
                           ("gtd/school.org" :maxlevel . 2)
                           ("gtd/rlist.org" :maxlevel . 2)
                           ("gtd/projects.org" :maxlevel . 2)
        )))
        ;;prot/scroll-center-cursor-mode t))

(remove-hook 'org-mode-hook #'auto-fill-mode)

(add-hook! org-mode :append
           #'visual-line-mode
           #'org-superstar-mode
           #'prettify-symbols-mode
           ;#'org-modern
           #'org-toggle-pretty-entities
           ;#'variable-pitch-mode
)
(setq-hook! org-mode line-spacing .1)

(custom-set-faces!
  '(org-todo          :family "FiraCode Nerd Font")
  '(org-done          :family "FiraCode Nerd Font" :strike-through t)
  '(org-headline-done :strike-through t)
  '(org-document-info-keyword :foreground "#676E95", :extend nil
                       :family "FiraCode Nerd Font" :weight regular)
  '(org-meta-line      :foreground "#676E95", :extend nil
                       :family "FiraCode Nerd Font" :weight regular)
  '(org-superstar-leading-bullet :family "FiraCode Nerd Font")
  '(org-latex-and-related :weight normal :foreground "#82aaff")
  '(org-date :weight normal :foreground "#82aaff")
  '(org-special-keyword :weight normal :height 0.75 ))

(custom-set-faces!
  '(org-block-begin-line  :background "#292D3E", :foreground nil, :extend nil
                          :family "FiraCode Nerd Font" :weight regular)
  '(org-block             :foreground unspecified, :extend nil
                          :family "FiraCode Nerd Font" :weight regular)
  '(org-block-end-line    :background "#292D3E", :foreground nil, :extend nil
                          :family "FiraCode Nerd Font" :weight regular))

;(setq org-hidden-keywords '(title)) ;; hide #+TITLE:
; alternatives:  '("◉" "◈" "○" "▷") ;; Set different bullets
(setq org-superstar-item-bullet-alist
         '((?- . ?•) (?+ . ?◉)) ;; Set different bullets for plain lists
      org-superstar-headline-bullets-list
         '("Ⅰ" "Ⅱ" "Ⅲ" "Ⅳ" "Ⅴ" "Ⅵ") ;; Set different bullets
      org-superstar-remove-leading-stars t)
      ;org-hide-leading-stars t)

(add-hook! org-mode :append
   (setq prettify-symbols-alist
        '(("TODO" . "T")
          ("NEXT" . "N")
          ("WAIT" . "W")
          ("DONE" . "D")
          ("[ ]" . "☐")
          ("[X]" . "☑")
          ("[-]" . "◩")
          ;("[#A]" . "")
          ;("[#B]" . "")
          ;("[#C]" . "")
          ("#+begin_src" . "»")
          ("#+end_src" . "«")
          ("#+BEGIN_SRC" . "»")
          ("#+END_SRC" . "«")
          ("#+BEGIN_EXPORT" . "»")
          ("#+END_EXPORT" . "«")
          ("#+RESULTS:" . "")
          (":PROPERTIES:" . "")
          ;("#+STARTUP:" . "")
          ;(":Effort:" . "")
          ("#+TITLE" . "󰬛")
          ("#+title" . "󰬛")
          ("#+SUBTITLE" . "󰬚")
          ("#+subtitle" . "󰬚")
          ("#+AUTHOR" . "󰬈")
          ("#+author" . "󰬈")
          ("#+DATE" . "󰬋")
          ("#+date" . "󰬋"))))
          ;("SCHEDULED:" . "")
          ;("DEADLINE:" . "")

(setq org-capture-templates '(
   ("t" "TODO" entry (file "gtd/inbox.org") "* TODO %?" :unnarrowed t)
   ( "r" "Reading list" )
   ("re" "Emacs" entry (file/headline "gtd/rlist.org" "Emacs") "* %?")
   ("ro" "Org Mode" entry (file+olp "gtd/rlist.org" "Org Mode" "Other") "* %?")
   ("ra" "Arch" entry (file+headline "gtd/rlist.org" "Arch") "* %?")
   ("rr" "Other" entry (file "gtd/rlist.org" ) "* %?")
))

(map! :map evil-org-mode-map :m :prefix "g"
      :m "h" 'evil-first-non-blank-of-visual-line
      :m "H" 'evil-org-top
      :m "K" 'org-up-element
      :m "k" 'org-backward-heading-same-level
      :m "J" 'org-down-element
      :m "j" 'org-forward-heading-same-level
      :m "l" 'evil-end-of-visual-line
      )

(map! :map evil-motion-state-map
      "j" 'evil-next-visual-line
      "k" 'evil-previous-visual-line
      )

(after! dired
   ;(setq dired-listing-switches "-Aphl -v --group-directories-first"
   (setq dired-listing-switches "-AFhl --group-directories-first"
        dired-recursive-copies 'always
        dired-recursive-deletes 'always))

(add-hook 'dired-mode-hook
      (lambda ()
        (dired-hide-details-mode)))
(setq dired-omit-files
  (rx (or (seq bol (? ".") "#") ;; emacs autosave files
   (seq bol "." (not (any "."))) ;; dot-files
   (seq "~" eol)                 ;; backup-files
   (seq bol "CVS" eol)           ;; CVS dirs
   )))

(map! :map dired-mode-map :n "v" 'dired-view-file)
(map! :map dired-mode-map :n "<mouse-2>" 'dired-find-file)
(map! :map view-mode-map :n "v" 'View-quit)
(map! :map pdf-view-mode-map :n "v" 'View-quit)

(setq-hook! dired-mode line-spacing 2900)
(setq-default line-spacing 20)
