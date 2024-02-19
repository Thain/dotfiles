(after! org
  (setq org-directory "~/org"
        ;; todo vars
        org-todo-keywords '((sequence "T(t)" "N(n)" "|" "W(w)" "D(d)" ))
        org-use-fast-todo-selection 'expert
        ;; startup vars
        org-startup-folded t
        org-startup-indented t
        org-hide-block-startup t
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
))

(remove-hook 'org-mode-hook #'auto-fill-mode)

(defun tangle-dots ()
  (when (or (equal (file-name-directory (directory-file-name buffer-file-name))
               (concat (getenv "HOME") "/dots/"))
            (equal (file-name-directory (directory-file-name buffer-file-name))
               (concat (getenv "HOME") "/dots/doom/")))
    (org-babel-tangle)
    (message "%s tangled" buffer-file-name)))
(defun set-tangle-dots ()
  (add-hook! 'after-save-hook :append :local #'tangle-dots))
(add-hook 'org-mode-hook #'set-tangle-dots)

(defun org-babel-execute:org (body params)
  "Return BODY with variables from PARAMS replaced by their values."
  (let* ((vars (cl-loop for par in params
            if (eq (car par) :var)
            collect (cons (symbol-name (cadr par)) (cddr par))))
     (re (regexp-opt (mapcar #'car vars) 'words))
     (pos 0))
    (while (string-match re body pos)
      (setq body (replace-match
          (format "%s" (cdr (assoc-string (match-string 0 body) vars)))
          nil nil
          body)))
    body))

(defadvice backward-kill-word (around delete-pair activate)
  (if (or
       (eq (char-syntax (char-before)) ?\()
       (eq (char-syntax (char-before)) ?\[)
       (eq (char-syntax (char-before)) ?\")
       (eq (char-syntax (char-before)) ?\{))
      (progn
        (backward-char 1)
        (message "hi")
        (save-excursion
          (forward-sexp 1)
          (delete-char -1))
        (forward-char 1)
        (append-next-kill)
        (kill-backward-chars 1))
    ad-do-it))

(add-hook! org-mode :append
           #'visual-line-mode
           #'org-appear-mode
           #'olivetti-mode
           #'org-superstar-mode
           #'org-toggle-pretty-entities
           #'variable-pitch-mode
           #'org-modern-mode
)

(custom-set-faces!
 '(org-document-title :family "Source Sans Pro" :height 2.5 :weight semibold)
 '(outline-1          :family "Source Sans Pro" :height 1.5 :weight semibold)
 '(outline-2          :family "Source Sans Pro" :height 1.3 :weight semibold)
 '(outline-3          :family "Source Sans Pro" :height 1.2 :weight semibold)
 '(outline-4          :family "Source Sans Pro" :height 1.1 :weight semibold)
 '(outline-5          :family "Source Sans Pro")
 '(outline-6          :family "Source Sans Pro")
 '(variable-pitch     :family "Source Sans Pro" :height 1.3  :weight regular)
 '(org-hide           :family "FiraCode Nerd Font" :height 0.8)
 )

(custom-set-faces!
 '(org-document-info-keyword :foreground "#676E95", :extend nil
                      :family "FiraCode Nerd Font" :weight regular)
 '(org-meta-line      :foreground "#676E95", :extend nil :height 0.8
                      :family "FiraCode Nerd Font" :weight regular)
 '(org-superstar-header-bullet :family "FiraCode Nerd Font")
 '(org-checkbox-statistics-todo :height 0.8)
 '(org-checkbox-statistics-done :height 0.8)
 '(org-tag   :family "FiraCode Nerd Font" :height 0.6)
 '(org-date  :family "FiraCode Nerd Font" :foreground nil)
 '(org-table :family "FiraCode Nerd Font" :height 0.8)
 ;'(org-table :family "FiraCode Nerd Font" :height 0.8 :inherit fixed-pitch)
 '(hl-line   :background nil)
 '(fixed-pitch        :family "FiraCode Nerd Font"   :height 0.8)
;'(org-special-keyword :family "FiraCode Nerd Font" :weight normal :height 0.75 )
;'(org-latex-and-related :family "FiraCode Nerd Font" :weight normal :foreground "#82aaff")
 '(org-block             :family "FiraCode Nerd Font" :height 0.8)
 '(org-block-begin-line  :family "FiraCode Nerd Font")
 '(org-block-end-line    :family "FiraCode Nerd Font"))

(defun block-bg-change ()
   (set-face-attribute 'org-block-begin-line nil :background (doom-color 'bg))
   (set-face-attribute 'org-block-end-line   nil :background (doom-color 'bg))
)

(add-hook 'org-mode-hook 'block-bg-change)

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
;(custom-set-faces! '(fringe :background (doom-color 'blue)))

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

(custom-set-faces!
  '(org-todo          :family "FiraCode Nerd Font")
  '(org-done          :family "FiraCode Nerd Font" :strike-through t)
  '(org-headline-done :strike-through t))

(setq org-modern-todo nil)
(add-hook! org-mode :append
   (setq prettify-symbols-alist
        '(
         ;("T" . "T") ("N" . "N") ("W" . "W") ("D" . "D")
         (":PROPERTIES:" . "")    ("#+PROPERTY:" . "")
         ;("[#A]" . "") ("[#B]" . "") ("[#C]" . "")
         ;("#+begin_src" . "»")    ("#+end_src" . "«")
         ;("#+BEGIN_SRC" . "»")    ("#+END_SRC" . "«")
         ;("#+BEGIN_EXPORT" . "»") ("#+END_EXPORT" . "«")
         ;("#+RESULTS:" . "")    ("#+STARTUP:" . "»")
         ;("#+STARTUP:" . "")     (":Effort:" . "")
         ;("[0/1]" . "󰽤") ("[1/1]" . "󰪥")
         ;("[1/2]" . "󰪡") ("[0/2]" . "󰽤") ("[2/2]" . "󰪥")
         ;("[0/3]" . "") ("[1/3]" . "󰫄") ("[2/3]" . "󰫆") ("[3/3]" . "󰫈")
         ;("[0/4]" . "󰽤") ("[1/4]" . "󰪟")
         ;("[2/4]" . "󰪡") ("[3/4]" . "󰪣") ("[4/4]" . "󰪥")
         ;("[0/6]" . "") ("[1/6]" . "󰫃") ("[2/6]" . "󰫄")
         ;("[3/6]" . "󰫅") ("[4/6]" . "󰫆") ("[5/6]" . "󰫇") ("[6/6]" . "󰫈")
         ;("[0/8]" . "󰽤") ("[1/8]" . "󰪞") ("[2/8]" . "󰪟") ("[3/8]" . "󰪠")
         ;("[4/8]" . "󰪡") ("[5/8]" . "󰪢") ("[6/8]" . "󰪣") ("[7/8]" . "󰪤")
         ;("[8/8]" . "󰪥")
         ;("SCHEDULED:" . "")      ("DEADLINE:" . "")
         ))
   (prettify-symbols-mode 1)
   )

(setq org-hidden-keywords '(title)) ;; hide #+TITLE:

(setq org-modern-star nil) ; alternatives:  '("◉" "◈" "○" "▷")
(setq org-superstar-headline-bullets-list '("󱂈" "󱂉" "󱂊" "󱂋" "󱂌" "󱂍")
      org-superstar-remove-leading-stars t)

(setq org-modern-list '((43 . "◦")
                        (45 . "•")
                        (42 . "–"))
  org-modern-checkbox '((88 . "☑")
                        (45 . "◩")
                        (32 . "□")))

(setq org-modern-progress nil)

(setq org-modern-tag nil)

(after! org
   (setq org-capture-templates '(
      ("t" "T" entry (file "gtd/inbox.org") "* T %?" :unnarrowed t)
      ( "r" "Reading list" )
      ("re" "Emacs" entry (file/headline "gtd/rlist.org" "Emacs") "* %?")
      ("ro" "Org Mode" entry (file+olp "gtd/rlist.org" "Org Mode" "Other") "* %?")
      ("ra" "Arch" entry (file+headline "gtd/rlist.org" "Arch") "* %?")
      ("rr" "Other" entry (file "gtd/rlist.org" ) "* %?")
   )))

(defun make-refile-targets (dir)
  ;; takes as input a directory, and makes an alist of all org files in the
  ;; directory, associated with a specification of maximal level 2.
  (let* ((files (directory-files dir 'full (rx ".org" eos)))
         (maxlev (make-list (length files) '(:maxlevel . 2))))
    (cl-pairlis files maxlev)))


(after! org
    (setq org-refile-targets (append
                               (make-refile-targets "~/dots/doom")
                               (make-refile-targets "~/org/gtd")))
  )

(setq org-archive-location "~/.org-archive/%s_archive::")

(after! org
  (setq org-agenda-skip-scheduled-if-done t
        org-agenda-files (list
                          (concat org-directory "/gtd/")
                          (concat org-directory "/calendar/"))
        org-agenda-window-setup 'other-tab
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

(custom-set-faces!
 '(org-agenda-structure :family "Source Sans Pro" :height 2.5 :weight semibold)
 )

(add-hook! org-agenda-mode
           #'olivetti-mode
          )

(setq org-agenda-custom-commands
  '(("z" "My Agenda"
     ((todo "N"
            ((org-agenda-overriding-header "Next Tasks\n")))
      (tags-todo "-tickler/T|W"
           ((org-agenda-overriding-header "Task Stack\n")))
      (agenda ""
           ((org-agenda-span 7)
            (org-agenda-start-day "+1d")
            (org-agenda-overriding-header "Upcoming\n")
            (org-agenda-show-log nil)))))
    ("n" "My Next Tasks"
     ((todo "N"
        ((org-agenda-overriding-header "\nTasks\n"))
     ))
   ))
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

(defun org-insert-link--add-space-after (&rest args)
     (let ((is-in-evil-normal-mode
              (and (bound-and-true-p evil-mode)
                   (not (bound-and-true-p evil-insert-state-minor-mode))
                   (looking-at "[[:blank:]]"))))
       (if is-in-evil-normal-mode (insert " "))))
(advice-add 'org-insert-link :before 'org-insert-link--add-space-after)
(advice-add 'custom-org-cliplink :around 'org-insert-link--add-space-after)

(defun custom-org-cliplink ()
  (interactive)
  (org-cliplink-insert-transformed-title
   (org-cliplink-clipboard-content)     ;take the URL from the CLIPBOARD
   (lambda (url title)
     (let* ((parsed-url (url-generic-parse-url url)) ;parse the url
            (clean-title
             (cond ;; if the host is github.com, cleanup the title
              ((string= (url-host parsed-url) "github.com")
               (replace-regexp-in-string ".*\/\\(.*\\)" "\\1" title))
              ;; otherwise keep the original title
              (t title))))
       ;; forward the title to the default org-cliplink transformer
       (org-cliplink-org-mode-link-transformer url clean-title)))))

(map! :map evil-org-mode-map :m :prefix "g"
      ;; left and right
      :m "h" 'evil-first-non-blank-of-visual-line
      :m "l" 'evil-end-of-visual-line
      ;; up and down
      :m "H" 'evil-org-top
      :m "K" 'org-up-element
      :m "k" 'org-backward-heading-same-level
      :m "J" 'org-down-element
      :m "j" 'org-forward-heading-same-level
      )

(map! :map evil-motion-state-map
      "j" 'evil-next-visual-line
      "k" 'evil-previous-visual-line
      )

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

(provide 'org-mode)
