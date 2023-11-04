(after! org
  (setq org-directory "~/documents/org"
        ;; todo vars
        org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "WAIT(w)" "DONE(d)" ))
        ;;org-todo-keyword-faces '(("TODO" . "magenta"))
        org-use-fast-todo-selection 'expert
        ;; agenda vars
        org-agenda-skip-scheduled-if-done t
        org-agenda-files (list (concat org-directory "/gtd/" ))
        ;; startup vars
        org-startup-folded t
        org-startup-indented t
        ;; org-startup-with-inline-images t
        ;; fontifying
        org-fontify-done-headline t
        org-fontify-whole-heading-line t
        org-fontify-quote-and-verse-blocks t
        org-src-fontify-natively t
        ;; other
        org-cycle-include-plain-lists 'integrate ; plain lists start folded
        org-tags-column -60
        org-ellipsis "  " ;; folding symbol
        org-agenda-block-separator ""
        org-list-allow-alphabetical t   ; have a. A. a) A) list bullets
        ;;org-fold-catch-invisible-edits 'smart ; don't brazenly edit things you can't see
        org-archive-location "~/documents/.org-archive/%s_archive::"
        org-refile-targets '(("~/documents/org/gtd/admin.org" :maxlevel . 2)
                             ("~/documents/org/gtd/school.org" :maxlevel . 2)
                             ("~/documents/org/gtd/rlist.org" :maxlevel . 2)
                             ("~/documents/org/gtd/projects.org" :maxlevel . 2)
                            )
))

(remove-hook 'org-mode-hook #'auto-fill-mode)

(defun tangle-dots ()
  (when (or (equal (file-name-directory (directory-file-name buffer-file-name))
               (concat (getenv "HOME") "/dots/"))
            (equal (file-name-directory (directory-file-name buffer-file-name))
               (concat (getenv "HOME") "/dots/doom/")))
    (org-babel-tangle)
    (message "%s tangled" buffer-file-name)))

(add-hook 'org-mode-hook
          (lambda () (add-hook 'after-save-hook #'tangle-dots
                          :append :local)))

(add-hook! org-mode :append
           #'visual-line-mode
           #'org-appear-mode
           #'olivetti-mode
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
  '(org-headline-done :strike-through t))

(custom-set-faces!
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

(add-hook 'org-mode-hook (lambda ()
  (display-line-numbers-mode -1)
  (setq left-margin-width 2)
  (setq right-margin-width 2)
  (vi-tilde-fringe-mode -1)))

(use-package! olivetti
  :after org olivetti
  :config
    ;(setq olivetti-min-body-width 110
    (setq olivetti-body-width 100
          olivetti-style 'fancy ; fantastic new layout
          olivetti-margin-width 6))

(custom-set-faces! '(fringe :background "#272B3A"))

(setq org-hide-emphasis-markers t
      org-entities-user
    '(("R"            "\\mathbb{R}" t   "&#82;" "R" "R" "ℝ")
      ("C"            "\\mathbb{C}" t   "&#67;" "C" "C" "ℂ")
      ("N"            "\\mathbb{N}" t   "&#78;" "N" "N" "ℕ")
      ("Z"            "\\mathbb{Z}" t   "&#90;" "Z" "Z" "ℤ")
      ("mapsto"       "\\mapsto"    t   "&#8614;" "mapsto" "mapsto" "↦")
      ("contra"       "\\lightning"  nil "&#11085;" "contra" "contra" "↯")
      ("Rarrow"       "\\Rightarrow"  t "&#8658;" "=>" "=>" "⇒")
      ("inj"          "\\hookrightarrow" t "&#8618;" "inj" "inj" "↪")
      ;("yo"           "\\yo"           nil "&#x3088;" "y" "y" "よ")
      ("f"            "\\textit{f}" nil "&fnof;" "f" "f" "ƒ")))

  (setq org-appear-autoemphasis t   ; need org-hide-emphasis-markers
        org-appear-autosubmarkers t ; need org-pretty-entities
        org-appear-autoentities t   ; need org-pretty-entities
  )

(add-hook! org-mode :append
   (setq prettify-symbols-alist
        '(("TODO" . "T")
          ("NEXT" . "N")
          ("WAIT" . "W")
          ("DONE" . "D")
          ("[ ]" . "☐")
          ("[X]" . "◩")
          ("[-]" . "☑")
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
          ("#+date" . "󰬋")
          ("[0/1]" . "󰽤")
          ("[1/1]" . "󰪥")
          ("[1/2]" . "󰪡")
          ("[0/2]" . "󰽤")
          ("[2/2]" . "󰪥")
          ("[0/3]" . "")
          ("[1/3]" . "󰫄")
          ("[2/3]" . "󰫆")
          ("[3/3]" . "󰫈")
          ("[0/4]" . "󰽤")
          ("[1/4]" . "󰪟")
          ("[2/4]" . "󰪡")
          ("[3/4]" . "󰪣")
          ("[4/4]" . "󰪥")
          ("[0/6]" . "")
          ("[1/6]" . "󰫃")
          ("[2/6]" . "󰫄")
          ("[3/6]" . "󰫅")
          ("[4/6]" . "󰫆")
          ("[5/6]" . "󰫇")
          ("[6/6]" . "󰫈")
          ("[0/8]" . "󰽤")
          ("[1/8]" . "󰪞")
          ("[2/8]" . "󰪟")
          ("[3/8]" . "󰪠")
          ("[4/8]" . "󰪡")
          ("[5/8]" . "󰪢")
          ("[6/8]" . "󰪣")
          ("[7/8]" . "󰪤")
          ("[8/8]" . "󰪥"))))
          ;("SCHEDULED:" . "")
          ;("DEADLINE:" . "")

;(setq org-hidden-keywords '(title)) ;; hide #+TITLE:

; alternatives:  '("◉" "◈" "○" "▷") ;; Set different bullets
(setq org-superstar-item-bullet-alist
         '((?- . ?•) (?+ . ?◉)) ;; Set different bullets for plain lists
      org-superstar-headline-bullets-list
         ;'(" ") ;; Set different bullets
         '("󱂈" "󱂉" "󱂊" "󱂋" "󱂌" "󱂍") ;; Set different bullets
      org-hide-leading-stars t)

(setq org-capture-templates '(
   ("t" "TODO" entry (file "gtd/inbox.org") "* TODO %?" :unnarrowed t)
   ( "r" "Reading list" )
   ("re" "Emacs" entry (file/headline "gtd/rlist.org" "Emacs") "* %?")
   ("ro" "Org Mode" entry (file+olp "gtd/rlist.org" "Org Mode" "Other") "* %?")
   ("ra" "Arch" entry (file+headline "gtd/rlist.org" "Arch") "* %?")
   ("rr" "Other" entry (file "gtd/rlist.org" ) "* %?")
))

(after! org
  (setq org-agenda-skip-scheduled-if-done t
        org-agenda-files (list ( concat org-directory "/gtd/" ))
        ;org-agenda-block-separator ""
        ;; styling
        org-agenda-tags-column 'auto
        org-agenda-block-separator ?─
        org-agenda-time-grid
        '((daily today require-timed)
          (800 1000 1200 1400 1600 1800 2000)
          " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
        org-agenda-current-time-string
          "⭠ now ─────────────────────────────────────────────────"
))

(map! :map evil-org-agenda-mode-map
      :m "q" 'org-agenda-quit
      :m "Q" 'org-agenda-exit
      )

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

;(setq-default org-display-custom-times t)
;(setq org-time-stamp-custom-formats '("<%a %b %e>" . "<%a %b %e %Y %H:%M>"))
(setq cfw:event-format-detail "%s - %e : %l")
(setq cfw:event-format-overview "%s - %e : %l")

(setq calendar-holidays '((holiday-fixed 1 1 "New Year's Day")
        (holiday-float 1 1 3 "Martin Luther King Day")
        (holiday-fixed 2 2 "Groundhog Day")
        (holiday-fixed 2 14 "Valentine's Day")
        (holiday-float 2 1 3 "President's Day")
        (holiday-fixed 3 17 "St. Patrick's Day")
        (holiday-fixed 4 1 "April Fools' Day")
        (holiday-float 5 0 2 "Mother's Day")
        (holiday-float 5 1 -1 "Memorial Day")
        (holiday-fixed 6 14 "Flag Day")
        (holiday-float 6 0 3 "Father's Day")
        (holiday-fixed 7 4 "Independence Day")
        (holiday-float 9 1 1 "Labor Day")
        (holiday-float 10 1 2 "Columbus Day")
        (holiday-fixed 10 31 "Halloween")
        (holiday-fixed 11 11 "Veteran's Day")
        (holiday-float 11 4 4 "Thanksgiving")
        (holiday-easter-etc)
        (holiday-fixed 12 25 "Christmas")
        (holiday-chinese-new-year)
        (if calendar-chinese-all-holidays-flag
            (append
             (holiday-chinese 1 15 "Lantern Festival")
             (holiday-chinese-qingming)
             (holiday-chinese 5 5 "Dragon Boat Festival")
             (holiday-chinese 7 7 "Double Seventh Festival")
             (holiday-chinese 8 15 "Mid-Autumn Festival")
             (holiday-chinese 9 9 "Double Ninth Festival")
             (holiday-chinese-winter-solstice)))
        (solar-equinoxes-solstices)
        (holiday-sexp calendar-daylight-savings-starts
                      (format "Daylight Saving Time Begins %s"
                              (solar-time-string
                               (/ calendar-daylight-savings-starts-time
                                  (float 60))
                               calendar-standard-time-zone-name)))
        (holiday-sexp calendar-daylight-savings-ends
                      (format "Daylight Saving Time Ends %s"
                              (solar-time-string
                               (/ calendar-daylight-savings-ends-time
                                  (float 60))
                               calendar-daylight-time-zone-name)))))

(setq org-caldav-url "https://cloud.thain.xyz/remote.php/dav/calendars/liam"
      org-icalendar-timezone "Europe/Amsterdam")

(setq org-caldav-calendars
  '((:calendar-id "personal"
     :files (( concat org-directory "/calendar/personal.org" ))
     :inbox (file+headline ( concat org-directory "/calendar/personal.org" ) "Inbox"))
     ;;:inbox "~/documents/org/calendar/personal.org")
    (:calendar-id "class"
     :files (( concat org-directory "/calendar/class.org" ))
     :inbox ( concat org-directory "/calendar/class.org" ))))

(after! org-journal
  (setq org-journal-dir (concat org-directory "/journal/")
        org-journal-file-type 'weekly))

;; org-journal keybinds
(map! :leader
      (:prefix ("j" . "journal")
        :desc "New Entry"           "j" #'org-journal-new-entry
        :desc "Open Journal"        "o" #'org-journal-open-current-journal-file
        :desc "Save and Exit"       "d" #'(lambda () (interactive) (save-buffer) (kill-buffer-and-window))
        :desc "Next Entry"          "n" #'org-journal-next-entry
        :desc "Previous Entry"      "p" #'org-journal-previous-entry))
