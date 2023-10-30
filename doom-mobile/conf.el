(setq user-full-name "Liam Chung"
      user-mail-address "lwalkerchung@gmail.com")

(defun a-very-pretty-flower ()
  (let* ((banner '("                              ....                                  "
                   "                           ,;;\'\'\'\';;,                    ,;;;;,     "
                   "                 ,        ;;\'      `;;,               .,;;;\'   ;    "
                   "              ,;;;       ;;          `;;,\';;;,.     ,%;;\'     \'     "
                   "            ,;;,;;       ;;         ,;`;;;, `;::.  %%;\'             "
                   "           ;;;,;;;       `\'       ,;;; ;;,;;, `::,%%;\'              "
                   "           ;;;,;;;,          .,%%%%%\'% ;;;;,;;   %;;;               "
                   " ,%,.      `;;;,;;;,    .,%%%%%%%%%\'%; ;;;;;,;;  %;;;               "
                   ";,`%%%%%%%%%%`;;,;;\'%%%%%%%%%%%%%\'%%\'  `;;;;;,;, %;;;               "
                   ";;;,`%%%%%%%%%%%,; ..`%%%%%%%%;\'%%%\'    `;;;;,;; %%;;               "
                   " `;;;;;,`%%%%%,;;/, .. `\"\"\"\'\',%%%%%      `;;;;;; %%;;,              "
                   "    `;;;;;;;,;;/////,.    ,;%%%%%%%        `;;;;,`%%;;              "
                   "           ;;;/%%%%,%///;;;\';%%%%%%,          `;;;%%;;,             "
                   "          ;;;/%%%,%%%%%/;;;\';;\'%%%%%,             `%%;;             "
                   "         .;;/%%,%%%%%//;;\'  ;;;\'%%%%%,             %%;;,            "
                   "         ;;//%,%%%%//;;;\'   `;;;;\'%%%%             `%;;;            "
                   "         ;;//%,%//;;;;\'      `;;;;\'%%%              %;;;,           "
                   "         `;;//,/;;;\'          `;;;\'%%\'              `%;;;           "
                   "           `;;;;\'               `;\'%\'                `;;;;          "
                   "                                  \'      .,,,.        `;;;;         "
                   "                                      ,;;;;;;;;;;,     `;;;;        "
                   "                                     ;;;\'    ;;;,;;,    `;;;;       "
                   "                                     ;;;      ;;;;,;;.   `;;;;      "
                   "                                      `;;      ;;;;;,;;   ;;;;      "
                   "                                        `\'      `;;;;,;;  ;;;;      "
                   "                                                   `;;,;, ;;;;      "
                   "                                                      ;;, ;;;;      "
                   "                                                        \';;;;;      "
                   "                                                         ;;;;;      "
                   "                                                        .;;;;\'      "
                   "                                                       .;;;;\'       "
                   "                                                      ;;;;;\'        "
                   "                                                    ,;;;;\'          "))
         (longest-line (apply #'max (mapcar #'length banner))))
    (put-text-property
     (point)
     (dolist (line banner (point))
       (insert (+doom-dashboard--center
                +doom-dashboard--width
                (concat line (make-string (max 0 (- longest-line (length line))) 32)))
               "\n"))
     'face 'doom-dashboard-banner)))

; (setq +doom-dashboard-ascii-banner-fn #'doom-dashboard-draw-ascii-emacs-banner-fn)
(setq +doom-dashboard-ascii-banner-fn #'a-very-pretty-flower)
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-footer)
(setq-hook! '+doom-dashboard-mode-hook cursor-type nil)
(setq-hook! '+doom-dashboard-mode-hook evil-normal-state-cursor nil)

;;alternatives: doom-dracula, doom-one, doom-solarized-dark, doom-palenight
;(setq doom-theme 'doom-nord)
;(setq doom-theme 'doom-nano-dark)
(setq doom-theme 'doom-palenight)
(setq projectile-project-search-path '("~/documents" ) ;; default project dir
      line-move-visual nil ;; no visual line mode, ie moving across wrapped lines are separate
      display-line-numbers-type t ;; line numbers: nil for none, relative, or t for reg
      ;doom-font (font-spec :family "JetBrains Mono" :size 22)
      ;doom-font (font-spec :family "Roboto Mono" :size 22)
      doom-font (font-spec :family "FiraCode Nerd Font" :size 16 :weight 'regular)
      confirm-kill-emacs nil) ;; do not ask if i want to exit

;; i think it should work better now since i put snippets back in packages.el?
;;(setq doom-snippets-dir "~/.doom.d/snippets/")
;;(setq +file-templates-dir "~/.doom.d/snippets/")
(set-file-template! "\\.tex$" :trigger "latex-default")
;;(yas-reload-all)

;; when opening file, read number of lines and add width to line number column
(defun display-line-numbers-equalize ()
  (setq display-line-numbers-width (length (number-to-string (line-number-at-pos (point-max))))))
(add-hook 'find-file-hook 'display-line-numbers-equalize)

;; Hide the tabs since I don't want to click on them
(setq tab-bar-show nil
      desktop-save-mode t)

; (use-package lambda-line
;   :custom
;   (lambda-line-icon-time nil) ;; requires ClockFace font (see below)
;   (lambda-line-position 'top) ;; Set position of status-line
;   (lambda-line-abbrev t) ;; abbreviate major modes
;   (lambda-line-hspace " ")  ;; add some cushion
;   (lambda-line-prefix t) ;; use a prefix symbol
;   (lambda-line-prefix-padding nil) ;; no extra space for prefix
;   (lambda-line-status-invert nil)  ;; no invert colors
;   (lambda-line-gui-ro-symbol  " ◉") ;; symbols
;   (lambda-line-gui-mod-symbol " ●")
;   (lambda-line-gui-rw-symbol  " ○")
;   (lambda-line-space-top +.60)  ;; padding on top and bottom of line
;   (lambda-line-space-bottom -.60)
;   (lambda-line-symbol-position 0) ;; adjust the vertical placement of symbol
;   :config
;   ;; activate lambda-line
;   (lambda-line-mode)
;   ;; set divider line in footer
;   (when (eq lambda-line-position 'top)
;     (setq-default mode-line-format (list "%_"))
;     (setq mode-line-format (list "%_"))))

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
        :desc "Open dired in org dir"         "o" (lambda () (interactive) (dired "~/storage/shared/Documents/org"))
        :desc "Close all dired buffers"       "q" #'+dired/quit-all
        :desc "Open dired in PWD"             "d" #'dired-jump))

(map! :leader
      (:prefix ("o" . "open")
        :desc "Open agenda"                   "a" #'org-agenda
        :desc "Open terminal"                 "s" #'vterm
        :desc "Open media player"             "e" #'emms
        :desc "Open calendar"                 "c" #'=calendar
        :desc "Open notes (notes.org)"        "n" (lambda () (interactive) (find-file "~/storage/shared/Documents/org/notes.org"))
        :desc "Open inbox"                    "i" (lambda () (interactive) (find-file "~/storage/shared/Documents/org/gtd/inbox.org"))
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

(map! :leader
      (:prefix ("e" . "emms")
        :desc "Open emms buffer"         "o" #'emms
        :desc "Open emms buffer"         "e" #'emms
        :desc "Open smart browser"       "B" #'emms-smart-browse
        :desc "Open normal browser"      "b" #'emms-browser
        :desc "Clear playlist"           "c" #'emms-playlist-current-clear
        :desc "Pause / Resume"           "p" #'emms-pause
        :desc "Start"                    "s" #'emms-start
        :desc "Stop"                     "t" #'emms-stop
        :desc "Next track"               "." #'emms-next
        :desc "Prev track"               "," #'emms-previous
        (:prefix ("a" . "add")
                :desc "Add dir"                  "d" #'emms-add-directory
                :desc "Add file"                 "f" #'emms-add-file
                :desc "Cache all files in dir"   "c" #'emms-add-directory-tree
                :desc "Add playlist"             "p" #'emms-add-playlist)))

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
        org-agenda-files (list "~/storage/shared/Documents/org/gtd/")
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
        org-ellipsis " [±] " ;; folding symbol
        org-agenda-block-separator ""
        org-list-allow-alphabetical t   ; have a. A. a) A) list bullets
        org-fold-catch-invisible-edits 'smart ; don't brazenly edit things you can't see

        org-refile-targets '(("~/storage/shared/Documents/org/gtd/gtd.org" :maxlevel . 2)
                           ("~/storage/shared/Documents/org/gtd/someday.org" :level . 2))
        ))
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
  '(org-headline-done :strike-through t))

;(custom-set-faces!
 ;'(outline-8          :family "ETBembo" :weight bold)
 ;'(outline-7          :family "ETBembo" :weight bold)
 ;'(outline-6          :family "ETBembo" :weight bold)
 ;'(outline-5          :family "ETBembo" :weight bold)
 ;'(outline-4          :family "ETBembo" :weight bold :height 1.2)
 ;'(outline-3          :family "ETBembo" :weight bold :height 1.4)
 ;'(outline-2          :family "ETBembo" :weight bold :height 1.6)
 ;'(outline-1          :family "ETBembo" :weight bold :height 1.8)
 ;'(org-document-title :family "ETBembo" :weight bold :height 2.5 :underline nil)
 ;'(variable-pitch     :family "ETBembo" :height 150 :weight book)
 ;'(org-hide           :family "FiraCode Nerd Font" :height 0.8)

 ;'(outline-4          :family "FiraCode Nerd Font" :weight bold :height 1.1)
 ;'(outline-3          :family "FiraCode Nerd Font" :weight bold :height 1.4)
 ;'(outline-2          :family "FiraCode Nerd Font" :weight bold :height 1.5)
 ;'(outline-1          :family "FiraCode Nerd Font" :weight bold :height 1.6)
 ;'(org-document-title :family "FiraCode Nerd Font" :weight bold :height 2.2 :underline nil)
 ;'(org-document-info-keyword :foreground "#676E95", :extend nil
 ;                     :family "FiraCode Nerd Font" :height 200 :weight regular)
 ;'(org-meta-line      :foreground "#676E95", :extend nil
 ;                     :family "FiraCode Nerd Font" :height 100 :weight regular)
 ;'(org-superstar-leading-bullet :family "FiraCode Nerd Font")
 ;'(org-checkbox-statistics-todo :height 1.1)
 ;'(org-latex-and-related :family "FiraCode Nerd Font" :weight normal :foreground "#82aaff")
 ;'(org-date :family "FiraCode Nerd Font" :weight normal :foreground "#82aaff")
 ;'(org-table :family "FiraCode Nerd Font")
 ;'(org-special-keyword :family "FiraCode Nerd Font" :weight normal :height 0.75 )
 ;'(fixed-pitch        :family "FiraCode Nerd Font"   :height 100))

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
        ;'(("TODO" . "○")
        ;  ("WAIT" . "○")
        ;  ("INACTIVE" . "○")
        ;  ("DONE" . "◉")
        '(("TODO" . "T")
          ("WAIT" . "W")
          ("INACTIVE" . "I")
          ("DONE" . "D")
          ("[ ]" . "") ;alts: ☐ ◩ ☑
          ("[X]" . "")
          ("[-]" . "")
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

(setq org-capture-templates
       '(("n" "note" entry (file+headline "~/storage/shared/Documents/org/notes.org" "Inbox"))
         ("s" "school" entry (file+headline "~/storage/shared/Documents/org/gtd/inbox.org" "School"))
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

(setq org-latex-default-packages-alist nil
      org-latex-toc-command nil
      org-latex-hyperref-template nil)
      org-format-latex-options (plist-put org-format-latex-options :scale 2.0)

(require 'ox-extra)
(ox-extras-activate '(ignore-headlines))

(eval-after-load 'org
  '(setf org-highlight-latex-and-related '(latex)))

(setq org-latex-classes
   '(("my-article" "
\\documentclass[10pt,a4paper]{article}
\\include{~/.config/latex/prelude}

\\usepackage{hyperref}
\\hypersetup{
  colorlinks=true,
  linkcolor=[rgb]{0,0.37,0.53},
  citecolor=[rgb]{0,0.47,0.68},
  filecolor=[rgb]{0,0.37,0.53},
  urlcolor=[rgb]{0,0.37,0.53},
  pagebackref=true,
  linktoc=all,}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}")
)))

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
     :files ("~/documents/org/calendar/personal.org")
     :inbox (file+headline "~/documents/org/calendar/personal.org" "Inbox"))
     ;;:inbox "~/documents/org/calendar/personal.org")
    (:calendar-id "class"
     :files ("~/documents/org/calendar/class.org")
     :inbox "~/documents/org/calendar/class.org")))

(after! dired
   ;(setq dired-listing-switches "-Aphl -v --group-directories-first"
   (setq dired-listing-switches "-AFhl --group-directories-first"
        dired-recursive-copies 'always
        dired-recursive-deletes 'always))

(add-hook 'dired-mode-hook
      (lambda ()
        (dired-sort-toggle-or-edit)))

(add-hook 'dired-mode-hook
      (lambda ()
        (dired-hide-details-mode)))
(setq dired-omit-files
    (rx (or (seq bol (? ".") "#")     ;; emacs autosave files
        (seq bol "." (not (any "."))) ;; dot-files
        (seq "~" eol)                 ;; backup-files
        (seq bol "CVS" eol)           ;; CVS dirs
        )))

(map! :map dired-mode-map :n "v" 'dired-view-file)
(map! :map view-mode-map :n "v" 'View-quit)
(map! :map pdf-view-mode-map :n "v" 'View-quit)
